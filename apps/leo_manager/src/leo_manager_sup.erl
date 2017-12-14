%%======================================================================
%%
%% LeoManaegr
%%
%% Copyright (c) 2012-2017 Rakuten, Inc.
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
-module(leo_manager_sup).

-behaviour(supervisor).

-include("leo_manager.hrl").
-include("tcp_server.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("leo_s3_libs/include/leo_s3_auth.hrl").
-include_lib("leo_s3_libs/include/leo_s3_user.hrl").
-include_lib("eunit/include/eunit.hrl").


%% External API
-export([start_link/0, stop/0]).
-export([create_mnesia_tables/2, migrate_mnesia_tables/0]).


%% Callbacks
-export([init/1]).

-define(CHECK_INTERVAL, 3000).
-define(CHECK_INTERVAL_FOR_MNESIA, 500).
-define(ENV_REPLICA_PARTNER, 'partner').


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
%% @spec () -> ok
%% @doc start link...
%% @end
start_link() ->
    Mode = ?env_mode_of_manager(),
    Me = node(),

    {ReplicaNodes_1, ReplicaNodes_2} =
        case ?env_partner_of_manager_node() of
            [] ->
                {[Me] ,[{Mode, Me}]};
            Nodes_1 ->
                Nodes_2 = lists:map(fun(N) when is_atom(N) -> N;
                                       (N) -> list_to_atom(N)
                                    end, Nodes_1),
                {[Me|Nodes_2], [{Mode, Me},{?ENV_REPLICA_PARTNER, Nodes_2}]}
        end,

    %% Set mnesia's replica nodes in app-env
    leo_misc:init_env(),
    leo_misc:set_env(leo_redundant_manager, ?PROP_MNESIA_NODES, ReplicaNodes_1),

    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, Pid} ->
            %% Launch TCP-Server(s)
            CUI_Console  = #tcp_server_params{prefix_of_name  = "tcp_server_cui_",
                                              port = ?env_listening_port_cui(),
                                              num_of_listeners = ?env_num_of_acceptors_cui()},
            JSON_Console = #tcp_server_params{prefix_of_name  = "tcp_server_json_",
                                              port = ?env_listening_port_json(),
                                              num_of_listeners = ?env_num_of_acceptors_json()},
            PluginModConsole = ?env_plugin_mod_console(),
            ok = leo_manager_console:start_link(leo_manager_formatter_text, CUI_Console,  PluginModConsole),
            ok = leo_manager_console:start_link(leo_manager_formatter_json, JSON_Console, PluginModConsole),

            %% Launch the components
            ok = start_logger(),
            ok = leo_manager_mq_client:start(?MODULE, [], ?env_queue_dir()),
            ok = start_redundant_manager(Pid, Mode, ReplicaNodes_1),
            ok = start_s3libs(),
            ok = application:start(leo_rpc),

            %% Launch Mnesia and create that tables
            MnesiaDir = case application:get_env(mnesia, dir) of
                            {ok, Dir} ->
                                Dir;
                            undefined ->
                                ?DEF_MNESIA_DIR
                        end,
            case filelib:fold_files(MnesiaDir, "\\.DCD$", false,
                                    fun(X, Acc) ->
                                            [X|Acc]
                                    end, []) of
                [] ->
                    ok = leo_misc:startup_notification(),
                    timer:apply_after(?CHECK_INTERVAL, ?MODULE,
                                      create_mnesia_tables, [Mode, ReplicaNodes_2]);
                _ ->
                    create_mnesia_tables_2(Mode, ReplicaNodes_2)
            end,
            {ok, Pid};
        Error ->
            Error
    end.


%% @doc Create mnesia tables
-spec(create_mnesia_tables(master|slave, [atom()]) ->
             ok | {error, any()}).
create_mnesia_tables(_, []) ->
    {error, badarg};
create_mnesia_tables(Mode, ReplicaNodes) ->
    case leo_misc:get_value(?ENV_REPLICA_PARTNER, ReplicaNodes) of
        undefined ->
            create_mnesia_tables_1(Mode, ReplicaNodes);
        PartnerNodes ->
            case lists:foldl(fun(N, _) ->
                                     case catch net_adm:ping(N) of
                                         pong ->
                                             true;
                                         _  ->
                                             false
                                     end
                             end, false, PartnerNodes) of
                true ->
                    create_mnesia_tables_1(Mode, ReplicaNodes);
                false ->
                    timer:apply_after(?CHECK_INTERVAL, ?MODULE,
                                      create_mnesia_tables, [Mode, ReplicaNodes])
            end
    end.


%% @spec () -> ok |
%%             not_started
%% @doc stop process.
%% @end
stop() ->
    ok.


%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
%% @spec (Params) -> ok
%% @doc stop process.
%% @end
%% @private
init([]) ->
    ChildProcs = [
                  {tcp_server_sup,
                   {tcp_server_sup, start_link, []},
                   permanent,
                   ?SHUTDOWN_WAITING_TIME,
                   supervisor,
                   [tcp_server_sup]},

                  {leo_manager_cluster_monitor,
                   {leo_manager_cluster_monitor, start_link, []},
                   permanent,
                   ?SHUTDOWN_WAITING_TIME,
                   worker,
                   [leo_manager_cluster_monitor]},

                  {leo_manager_table_sync,
                   {leo_manager_table_sync, start_link, []},
                   permanent,
                   ?SHUTDOWN_WAITING_TIME,
                   worker,
                   [leo_manager_table_sync]},

                  {leo_manager_ring_sync,
                   {leo_manager_ring_sync, start_link, []},
                   permanent,
                   ?SHUTDOWN_WAITING_TIME,
                   worker,
                   [leo_manager_ring_sync]},

                  {leo_manager_del_bucket_handler,
                   {leo_manager_del_bucket_handler, start_link, []},
                   permanent,
                   ?SHUTDOWN_WAITING_TIME,
                   worker,
                   [leo_manager_del_bucket_handler]}
                 ],
    {ok, {_SupFlags = {one_for_one, ?MAX_RESTART, ?MAX_TIME}, ChildProcs}}.


%% ---------------------------------------------------------------------
%% Inner Function(s)
%% ---------------------------------------------------------------------
%% @doc Launch LeoLogger
%% @private
start_logger() ->
    LogDir = ?env_log_dir(),
    LogLevel = ?env_log_level(leo_manager),
    ok = leo_logger_api:new(
           LogDir, LogLevel, log_file_appender()),
    ok = leo_logger_api:new(?LOG_GROUP_ID_HISTORY, ?LOG_ID_HISTORY,
                                    LogDir, ?LOG_FILENAME_HISTORY),
    ok.


%% @doc Launch LeoRedundantManager
%% @private
start_redundant_manager(Pid, Mode, ReplicaNodes) ->
    SystemConf = leo_manager_api:load_system_config(),
    N = SystemConf#?SYSTEM_CONF.n,
    R = SystemConf#?SYSTEM_CONF.r,
    W = SystemConf#?SYSTEM_CONF.w,
    D = SystemConf#?SYSTEM_CONF.d,
    MDCR_N = SystemConf#?SYSTEM_CONF.num_of_dc_replicas,
    MDCR_R = SystemConf#?SYSTEM_CONF.mdcr_r,
    MDCR_W = SystemConf#?SYSTEM_CONF.mdcr_w,
    MDCR_D = SystemConf#?SYSTEM_CONF.mdcr_d,
    MembershipCallback = fun leo_manager_api:synchronize/1,

    %% Validate the local consistency level
    case (N < 1 orelse
          R < 1 orelse
          W < 1 orelse
          D < 1 orelse
          N < R orelse
          N < W orelse
          N < D) of
        true when Mode == ?MANAGER_TYPE_MASTER ->
            exit('invalid_consistency_level');
        _ ->
            %% Validate the consistency level for mdcr
            case (MDCR_N < 1 orelse
                  MDCR_R < 1 orelse
                  MDCR_W < 1 orelse
                  MDCR_D < 1 orelse
                  MDCR_N < MDCR_R orelse
                  MDCR_N < MDCR_W orelse
                  MDCR_N < MDCR_D) of
                true when Mode == ?MANAGER_TYPE_MASTER ->
                    exit('invalid_mdcr_consistency_level');
                _ ->
                    ChildSpec =
                        case Mode of
                            ?MANAGER_TYPE_MASTER ->
                                {leo_redundant_manager_sup,
                                 {leo_redundant_manager_sup, start_link,
                                  [?MONITOR_NODE,
                                   ReplicaNodes,
                                   ?env_queue_dir(leo_manager),
                                   [{version, 1},
                                    {cluster_id, SystemConf#?SYSTEM_CONF.cluster_id},
                                    {dc_id, SystemConf#?SYSTEM_CONF.dc_id},
                                    {n, N},
                                    {r, R},
                                    {w, W},
                                    {d, D},
                                    {bit_of_ring, SystemConf#?SYSTEM_CONF.bit_of_ring},
                                    {num_of_dc_replicas, MDCR_N},
                                    {num_of_rack_replicas, SystemConf#?SYSTEM_CONF.num_of_rack_replicas},
                                    {max_mdc_targets, SystemConf#?SYSTEM_CONF.max_mdc_targets},
                                    {mdcr_r, MDCR_R},
                                    {mdcr_w, MDCR_W},
                                    {mdcr_d, MDCR_D}
                                   ],
                                   MembershipCallback
                                  ]
                                 },
                                 permanent, 2000, supervisor, [leo_redundant_manager_sup]};
                            _ ->
                                {leo_redundant_manager_sup,
                                 {leo_redundant_manager_sup, start_link,
                                  [?MONITOR_NODE,
                                   ReplicaNodes,
                                   ?env_queue_dir(leo_manager),
                                   [],
                                   MembershipCallback
                                  ]
                                 },
                                 permanent, 2000, supervisor, [leo_redundant_manager_sup]}
                        end,
                    {ok, _} = supervisor:start_child(Pid, ChildSpec),
                    ok
            end
    end.


%% @doc Launch LeoS3Libs
%% @private
start_s3libs() ->
    case ?env_use_s3_api() of
        false ->
            void;
        true ->
            ok = leo_s3_libs:start(?MANAGER_TYPE_MASTER, [])
    end,
    ok.


%% @doc Create mnesia tables
%% @private
-spec(create_mnesia_tables_1(manager_type(), list()) ->
             ok | {error, any()}).
create_mnesia_tables_1(?MANAGER_TYPE_MASTER = Mode, Nodes) ->
    Nodes_1 = lists:flatten(lists:map(fun({_, N}) -> N end, Nodes)),
    case mnesia:create_schema(Nodes_1) of
        ok ->
            try
                %% create mnesia's schema
                rpc:multicall(Nodes_1, application, stop,  [mnesia], ?DEF_TIMEOUT),
                rpc:multicall(Nodes_1, application, start, [mnesia], ?DEF_TIMEOUT),

                %% create table into the mnesia
                leo_manager_mnesia:create_storage_nodes(disc_copies, Nodes_1),
                leo_manager_mnesia:create_gateway_nodes(disc_copies, Nodes_1),
                leo_manager_mnesia:create_rebalance_info(disc_copies, Nodes_1),
                leo_manager_mnesia:create_available_commands(disc_copies, Nodes_1),
                leo_manager_mnesia:create_del_bucket_state(disc_copies, Nodes_1),

                leo_cluster_tbl_conf:create_table(disc_copies, Nodes_1),
                leo_mdcr_tbl_cluster_info:create_table(disc_copies, Nodes_1),
                leo_mdcr_tbl_cluster_stat:create_table(disc_copies, Nodes_1),
                leo_mdcr_tbl_cluster_mgr:create_table(disc_copies, Nodes_1),
                leo_mdcr_tbl_cluster_member:create_table(disc_copies, Nodes_1),

                leo_cluster_tbl_ring:create_table_current(disc_copies, Nodes_1),
                leo_cluster_tbl_ring:create_table_prev(disc_copies, Nodes_1),
                leo_cluster_tbl_member:create_table(disc_copies, Nodes_1, ?MEMBER_TBL_CUR),
                leo_cluster_tbl_member:create_table(disc_copies, Nodes_1, ?MEMBER_TBL_PREV),

                %% Load from system-config and store it into the mnesia
                {ok, _SystemConf} = leo_manager_api:load_system_config_with_store_data(),

                %% Insert available commands
                ok = leo_manager_mnesia:update_available_commands(?env_available_commands()),

                %% Create S3-API related tables
                ok = create_s3api_related_tables(Nodes_1),

                create_mnesia_tables_2(Mode, Nodes)
            catch _:Reason ->
                    ?error("create_mnesia_tables_1/2", [{cause, Reason}])
            end,
            ok;
        {error,{_,{already_exists, _}}} ->
            create_mnesia_tables_2(Mode, Nodes);
        {_, Cause} ->
            timer:apply_after(?CHECK_INTERVAL, ?MODULE, create_mnesia_tables, [Mode, Nodes]),
            ?error("create_mnesia_tables_1/2", [{cause, Cause}]),
            {error, Cause}
    end;
create_mnesia_tables_1(slave,_Nodes) ->
    create_mnesia_tables_2(slave,_Nodes).

%% @doc Create mnesia tables and execute to migrate data
%% @private
-spec(create_mnesia_tables_2(manager_type(), list()) ->
             ok | {error, any()}).
create_mnesia_tables_2(Mode,_Nodes) ->
    case application:start(mnesia) of
        ok ->
            create_mnesia_tables_2(Mode);
        {error, {already_started, mnesia}} ->
            %% https://github.com/leo-project/leofs/issues/560
            %% happend when starting a cluster for the first time.
            %% as mnesia can be started via rpc:multicall at master at this point.
            create_mnesia_tables_2(Mode);
        Error ->
            ?error("create_mnesia_tables_2/2", [{cause, Error}]),
            init:stop()
    end.
create_mnesia_tables_2(Mode) ->
    Ret = case catch mnesia:system_info(tables) of
              {'EXIT', {aborted, Reason}} ->
                  ?error("create_mnesia_tables_2/1", [{cause, Reason}]),
                  {error, ?ERROR_MNESIA_GET_TABLE_INFO_ERROR};
              Tbls when length(Tbls) > 1 ->
                  ok = leo_misc:startup_notification(),
                  case load_tables(Tbls) of
                      ok ->
                          try
                              %% Execute to migrate data
                              case Mode of
                                  ?MANAGER_TYPE_MASTER ->
                                      ok = migrate_mnesia_tables();
                                  _ ->
                                      void
                              end,

                              %% Launch Statistics
                              ok = leo_statistics_api:start_link(leo_manager),
                              ok = leo_metrics_vm:start_link(
                                     ?SNMP_SYNC_INTERVAL_10S, (Mode == slave)),
                              ok
                          catch
                              _:Cause ->
                                  ?error("create_mnesia_tables_2/1", [{cause, Cause}]),
                                  {error, ?ERROR_MNESIA_PROC_FAILURE}
                          end;
                      {timeout, BadTabList} ->
                          Cause = ?ERROR_MNESIA_WAIT_FOR_TABLE_TIMEOUT,
                          ?error("create_mnesia_tables_2/1",
                                 [{cause, Cause},
                                  {bad_tables, BadTabList}]),
                          {error, Cause};
                      {error, Reason} ->
                          ?error("create_mnesia_tables_2/1", [{cause, Reason}]),
                          {error, ?ERROR_MNESIA_WAIT_FOR_TABLE_ERROR}
                  end;
              _Tbls ->
                  Cause = ?ERROR_TABLE_NOT_EXISTS,
                  ?error("create_mnesia_tables_2/1", [{cause, Cause}]),
                  {error, Cause}
          end,

    case Ret of
        ok ->
            ok;
        _ ->
            init:stop()
    end.


%% @doc Force load tables when the mnesia won't load due to the possibility split brain could happen
%% @private
-spec(force_load_tables(Tables) ->
             ok | {error, Cause} when Tables::list(atom()),
                                      Cause::any()).
force_load_tables([]) ->
    ok;
force_load_tables([schema|Rest]) ->
    %% must skip the schema itself
    force_load_tables(Rest);
force_load_tables([H|Rest]) ->
    case mnesia:force_load_table(H) of
        yes ->
            force_load_tables(Rest);
        Error ->
            Error
    end.


%% @doc Wrapper function to load tables
%% @private
-spec(load_tables(Tables) ->
             ok | {error, Cause} | {timeout, BadTabList} when Tables::list(atom()),
                                                              Cause::any(),
                                                              BadTabList::list(atom())).
load_tables(Tables) ->
    Args = init:get_plain_arguments(),
    Last = lists:last(Args),
    case Last of
        "force_load" ->
            force_load_tables(Tables);
        _ ->
            mnesia:wait_for_tables(Tables, timer:seconds(180))
    end.


%% @doc Function migrating datas
%%      Should be called on the leo_manager master after all partner nodes finished initialization processes
%% @private
-spec(migrate_mnesia_tables() ->
             ok | {error, any()}).
migrate_mnesia_tables() ->
    migrate_mnesia_tables(0).

-spec(migrate_mnesia_tables(RetryTimes) ->
             ok | {error, any()} when RetryTimes::non_neg_integer()).
migrate_mnesia_tables(?RETRY_TIMES) ->
    timer:apply_after(?CHECK_INTERVAL_FOR_MNESIA, ?MODULE,
                      migrate_mnesia_tables, []),
    ok;
migrate_mnesia_tables(RetryTimes) ->
    try
        ok = leo_manager_transformer:transform(),
        ok = leo_manager_api:update_mdc_items_in_system_conf(),
        ok
    catch
        _:Cause ->
            ?error("migrate_mnesia_tables/0", [{cause, Cause}]),
            timer:sleep(timer:seconds(1)),
            migrate_mnesia_tables(RetryTimes + 1)
    end.


%% @doc Create S3API-related tables
-spec(create_s3api_related_tables(Nodes) ->
             ok when Nodes::[atom()]).
create_s3api_related_tables(Nodes) ->
    create_s3api_related_tables(?env_use_s3_api(), Nodes).

%% @private
create_s3api_related_tables(false,_Nodes) ->
    ok;
create_s3api_related_tables(true, Nodes) ->
    %% Create S3-related tables
    leo_s3_auth:create_table(disc_copies, Nodes),
    leo_s3_endpoint:create_table(disc_copies, Nodes),
    leo_s3_bucket:create_table(disc_copies, Nodes),
    leo_s3_user:create_table(disc_copies, Nodes),
    leo_s3_user_credential:create_table(disc_copies, Nodes),

    %% Insert test-related values
    CreatedAt = leo_date:now(),
    ok = leo_s3_libs_data_handler:insert({mnesia, leo_s3_users},
                                         {[], #?S3_USER{id = ?TEST_USER_ID,
                                                        role_id = 9,
                                                        created_at = CreatedAt
                                                       }}),
    ok = leo_s3_libs_data_handler:insert({mnesia, leo_s3_user_credential},
                                         {[], #user_credential{user_id = ?TEST_USER_ID,
                                                               access_key_id = ?TEST_ACCESS_KEY,
                                                               created_at = CreatedAt
                                                              }}),
    ok = leo_s3_libs_data_handler:insert({mnesia, leo_s3_credentials},
                                         {[], #credential{access_key_id = ?TEST_ACCESS_KEY,
                                                          secret_access_key = ?TEST_SECRET_KEY,
                                                          created_at = CreatedAt
                                                         }}),
    %% Insert default s3-endpoint values
    leo_s3_endpoint:set_endpoint(?DEF_ENDPOINT_1),
    leo_s3_endpoint:set_endpoint(?DEF_ENDPOINT_2),
    ok.


%% @doc Get log-file appender from env
%% @private
-spec(log_file_appender() ->
             list()).
log_file_appender() ->
    case application:get_env(leo_manager, log_appender) of
        undefined   -> log_file_appender([], []);
        {ok, Value} -> log_file_appender(Value, [])
    end.

-spec(log_file_appender(list(), list()) ->
             list()).
log_file_appender([], []) ->
    [{?LOG_ID_FILE_INFO,  ?LOG_APPENDER_FILE},
     {?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}];
log_file_appender([], Acc) ->
    lists:reverse(Acc);
log_file_appender([{Type, _}|T], Acc) when Type == file ->
    log_file_appender(T, [{?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}|
                          [{?LOG_ID_FILE_INFO, ?LOG_APPENDER_FILE}|Acc]]).
