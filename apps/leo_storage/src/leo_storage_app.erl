%%======================================================================
%%
%% LeoFS Storage
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
-module(leo_storage_app).

-behaviour(application).

-include("leo_storage.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("leo_watchdog/include/leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application and Supervisor callbacks
-export([start/2, prep_stop/1, stop/1]).


%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
%% @doc Start the application
start(_Type, _Args) ->
    application:start(leo_watchdog),
    Res = leo_storage_sup:start_link(),
    after_proc(Res).

%% @doc Prepare stoping the application
prep_stop(_State) ->
    catch leo_object_storage_sup:stop(),
    catch leo_redundant_manager_sup:stop(),
    catch leo_mq_sup:stop(),
    catch leo_logger_sup:stop(),
    catch leo_storage_sup:stop(),
    ok.

%% @doc Stop the application
stop(_State) ->
    ok.


%% @doc Start mnesia and create tables
%% @private
start_mnesia() ->
    start_mnesia(0).

%% @private
start_mnesia(?RETRY_TIMES) ->
    {error, "Could not create mnesia's tables"};
start_mnesia(RetryTimes) ->
    try
        %% Create cluster-related tables
        application:ensure_started(mnesia),
        leo_cluster_tbl_conf:create_table(ram_copies, [node()]),
        leo_cluster_tbl_conf:create_table(ram_copies, [node()]),
        leo_mdcr_tbl_cluster_info:create_table(ram_copies, [node()]),
        leo_mdcr_tbl_cluster_stat:create_table(ram_copies, [node()]),
        leo_mdcr_tbl_cluster_mgr:create_table(ram_copies, [node()]),
        leo_mdcr_tbl_cluster_member:create_table(ram_copies, [node()]),
        ok
    catch
        _:_Cause ->
            timer:sleep(timer:seconds(1)),
            start_mnesia(RetryTimes + 1)
    end.


%% @doc Start statistics
%% @private
start_statistics() ->
    start_statistics(0).

%% @private
start_statistics(?RETRY_TIMES) ->
    {error, "Launch failure of statistics"};
start_statistics(RetryTimes) ->
    try
        %% Launch metric-servers
        application:ensure_started(mnesia),
        application:ensure_started(snmp),

        leo_statistics_api:start_link(leo_storage),
        leo_statistics_api:create_tables(ram_copies, [node()]),
        leo_metrics_vm:start_link(?SNMP_SYNC_INTERVAL_10S),
        leo_metrics_req:start_link(?SNMP_SYNC_INTERVAL_60S),
        leo_storage_statistics:start_link(?SNMP_SYNC_INTERVAL_60S),
        ok
    catch
        _:_Cause ->
            start_statistics(RetryTimes + 1)
    end.


%%----------------------------------------------------------------------
%% INNER FUNCTION
%%----------------------------------------------------------------------
%% @private
ensure_started(Id, Module, Method, Type, Timeout) ->
    case whereis(Id) of
        undefined ->
            ChildSpec = {Id, {Module, Method, []},
                         permanent, Timeout, Type, [Module]},
            {ok, _} = supervisor:start_child(kernel_sup, ChildSpec);
        Pid -> Pid
    end.


%% @private
after_proc({ok, Pid}) ->
    ensure_started(inet_db, inet_db, start_link, worker, 2000),
    ensure_started(net_sup, erl_distribution, start_link, supervisor, infinity),

    %% Launch servers
    ok = launch_logger(),
    ok = launch_object_storage(Pid),
    ok = leo_ordning_reda_api:start(),

    %% Check the managers whether they are alive or not
    Managers = ?env_manager_nodes(leo_storage),
    IsAliveManagers = is_alive_managers(Managers),

    %% Launch others
    after_proc_1(IsAliveManagers, Pid, Managers);
after_proc(Error) ->
    ?error("after_proc/1", [{cause, Error}]),
    init:stop().

%% @private
after_proc_1(true, Pid, Managers) ->
    try
        QueueDir = ?env_queue_dir(leo_storage),
        ok = launch_redundant_manager(Pid, Managers, QueueDir),
        ok = leo_storage_mq:start(Pid, QueueDir),

        %% After processing
        ensure_started(rex, rpc, start_link, worker, 2000),
        ok = leo_storage_api:register_in_monitor(first),

        %% Launch leo-rpc
        ok = leo_rpc:start(),

        %% Watchdog for Storage in order to operate 'auto-compaction' automatically
        WatchdogInterval = ?env_storage_watchdog_interval(),
        case ?env_auto_compaction_enabled() of
            true ->
                {ok, _} = supervisor:start_child(
                            leo_watchdog_sup, {leo_storage_watchdog_fragment,
                                               {leo_storage_watchdog_fragment, start_link,
                                                [?env_warn_active_size_ratio(),
                                                 ?env_threshold_active_size_ratio(),
                                                 WatchdogInterval
                                                ]},
                                               permanent,
                                               2000,
                                               worker,
                                               [leo_storage_watchdog_fragment]});
            false ->
                void
        end,

        %% Watchdog for notified messages
        {ok, _} = supervisor:start_child(
                    leo_watchdog_sup, {leo_storage_watchdog_msgs,
                                       {leo_storage_watchdog_msgs, start_link,
                                        [?env_threshold_num_of_notified_msgs(),
                                         WatchdogInterval
                                        ]},
                                       permanent,
                                       2000,
                                       worker,
                                       [leo_storage_watchdog_msgs]}),
        ok = leo_storage_watchdog_sub:start(),

        %% Launch statistics/mnesia-related processes
        ok = start_mnesia(),
        ok = start_statistics(),
        {ok, Pid}
    catch
        _:Cause ->
            ?error("after_proc_1/3", [{cause, Cause}]),
            init:stop()
    end;
after_proc_1(false,_,Managers) ->
    ?error("after_proc_1/3", [{manager_nodes, Managers},
                              {cause, "Not alive managers"}]),
    init:stop().


%% @private
is_alive_managers([]) ->
    false;
is_alive_managers([Manager|Rest]) ->
    case leo_misc:node_existence(Manager) of
        true ->
            true;
        false ->
            is_alive_managers(Rest)
    end.


%% @doc Launch Logger
%% @private
launch_logger() ->
    DefLogDir = "./log/",
    LogDir = case application:get_env(leo_storage, log_appender) of
                 {ok, [{file, Options}|_]} ->
                     leo_misc:get_value(path, Options, DefLogDir);
                 _ ->
                     DefLogDir
             end,
    LogLevel = ?env_log_level(leo_storage),
    ok = leo_logger_client_message:new(LogDir, LogLevel, log_file_appender()),

    %% access-logger (file-appender)
    case application:get_env(leo_storage, is_enable_access_log) of
        {ok, true} ->
            ok = leo_logger_client_base:new(?LOG_GROUP_ID_ACCESS, ?LOG_ID_ACCESS,
                                            LogDir, ?LOG_FILENAME_ACCESS);
        _ ->
            void
    end,
    ok.


%% @doc Launch Object-Storage
%% @private
launch_object_storage(RefSup) ->
    ObjStoageInfo =
        case ?env_storage_device() of
            [] -> [];
            Devices ->
                lists:map(
                  fun(Item) ->
                          Containers = leo_misc:get_value(num_of_containers, Item),
                          Path       = leo_misc:get_value(path,              Item),
                          {Containers, Path}
                  end, Devices)
        end,

    ChildSpec = {leo_object_storage_sup,
                 {leo_object_storage_sup, start_link,
                  [ObjStoageInfo, leo_storage_msg_collector]},
                 permanent, 2000, supervisor, [leo_object_storage_sup]},
    {ok, _} = supervisor:start_child(RefSup, ChildSpec),
    ok.


%% @doc Launch redundnat-manager
%% @private
launch_redundant_manager(RefSup, Managers, QueueDir) ->
    ChildSpec = {leo_redundant_manager_sup,
                 {leo_redundant_manager_sup, start_link,
                  [?PERSISTENT_NODE, Managers, QueueDir]},
                 permanent, 2000, supervisor, [leo_redundant_manager_sup]},
    {ok, _} = supervisor:start_child(RefSup, ChildSpec),
    ok.


%% @doc Retrieve log-appneder(s)
%% @private
-spec(log_file_appender() ->
             list()).
log_file_appender() ->
    case application:get_env(leo_storage, log_appender) of
        undefined   -> log_file_appender([], []);
        {ok, Value} -> log_file_appender(Value, [])
    end.

log_file_appender([], []) ->
    [{?LOG_ID_FILE_INFO,  ?LOG_APPENDER_FILE},
     {?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}];
log_file_appender([], Acc) ->
    lists:reverse(Acc);
log_file_appender([{Type, _}|T], Acc) when Type == file ->
    log_file_appender(T, [{?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}|[{?LOG_ID_FILE_INFO, ?LOG_APPENDER_FILE}|Acc]]).
