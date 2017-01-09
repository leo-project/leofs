%%======================================================================
%%
%% Leo Gateway
%%
%% Copyright (c) 2012-2015 Rakuten, Inc.
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
%% ---------------------------------------------------------------------
%% Leo Gateway - Application
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_app).

-include("leo_gateway.hrl").
-include("leo_nfs_mount3.hrl").
-include("leo_nfs_proto3.hrl").
-include("leo_nlm_proto4.hrl").
-include("leo_http.hrl").
-include_lib("leo_cache/include/leo_cache.hrl").
-undef(error).
-undef(warn).
-undef(PROP_OPTIONS).
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("nfs_rpc_server/src/nfs_rpc_app.hrl").
-include_lib("leo_watchdog/include/leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

-behaviour(application).
-export([start/0, start/2, stop/1, prep_stop/1,
         inspect_cluster_status/2, profile_output/0, get_options/0]).

-define(CHECK_INTERVAL, 3000).


-ifdef(TEST).
-define(get_several_info_from_manager(_Args),
        fun() ->
                _ = get_system_config_from_manager([]),
                _ = get_members_from_manager([]),

                [
                 {ok, [#?SYSTEM_CONF{n = 1,
                                     w = 1,
                                     r = 1,
                                     d = 1}]},
                 {ok, {[#member{node  = 'node_0',
                                state = 'running'}],
                       [#member{node  = 'node_0',
                                state = 'running'}]}}
                ]
        end).
-else.
-define(get_several_info_from_manager(X),  {get_system_config_from_manager(X),
                                            get_members_from_manager(X)}).
-endif.

%% for debug
start() ->
    application:ensure_started(crypto),
    application:ensure_started(snmp),
    application:ensure_started(ssl),
    application:ensure_started(ranch),
    application:ensure_started(asn1),
    application:ensure_started(mnesia),
    application:start(leo_gateway).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for leo_gateway.
start(_Type, _StartArgs) ->
    consider_profiling(),
    application:start(leo_watchdog),
    App = leo_gateway,

    %% Launch Logger(s)
    DefLogDir = "./log/",
    LogDir    = case application:get_env(App, log_appender) of
                    {ok, [{file, Options}|_]} ->
                        leo_misc:get_value(path, Options,  DefLogDir);
                    _ ->
                        DefLogDir
                end,
    ok = leo_logger_client_message:new(LogDir, ?env_log_level(App), log_file_appender()),

    %% access-logger (file-appender)
    case application:get_env(leo_gateway, is_enable_access_log) of
        {ok, true} ->
            ok = leo_logger_client_base:new(?LOG_GROUP_ID_ACCESS, ?LOG_ID_ACCESS,
                                            LogDir, ?LOG_FILENAME_ACCESS);
        _ ->
            void
    end,

    %% Launch Supervisor
    Res = leo_gateway_sup:start_link(),
    after_process_0(Res).


%% @spec prep_stop(_State) -> ServerRet
%% @doc application stop callback for leo_gateway.
prep_stop(_State) ->
    catch leo_redundant_manager_sup:stop(),
    catch leo_mq_sup:stop(),
    catch leo_logger_sup:stop(),

    case catch get_options() of
        {ok, HttpOptions} ->
            case HttpOptions#http_options.handler of
                ?PROTO_HANDLER_S3 ->
                    leo_gateway_s3_api:stop();
                ?PROTO_HANDLER_REST ->
                    leo_gateway_rest_api:stop();
                _ ->
                    void
            end;
        _ ->
            void
    end,
    ok.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for leo_gateway.
stop(_State) ->
    ok.


-spec profile_output() -> ok.
profile_output() ->
    eprof:stop_profiling(),
    eprof:log("leo_gateway.procs.profile"),
    eprof:analyze(procs),
    eprof:log("leo_gateway.total.profile"),
    eprof:analyze(total).


-spec consider_profiling() -> profiling | not_profiling | {error, any()}.
consider_profiling() ->
    case application:get_env(leo_gateway, profile) of
        {ok, true} ->
            {ok, _Pid} = eprof:start(),
            eprof:start_profiling([self()]);
        _ ->
            not_profiling
    end.


%% @doc Inspect the cluster-status
%%
-spec(inspect_cluster_status(any(), list()) ->
             pid()).
inspect_cluster_status(Res, Managers) ->
    case ?get_several_info_from_manager(Managers) of
        {{ok, SystemConf}, {ok, {MembersCur, MembersPrev}}} ->
            case get_cluster_state(MembersCur) of
                ?STATE_STOP ->
                    timer:apply_after(?CHECK_INTERVAL, ?MODULE,
                                      inspect_cluster_status, [ok, Managers]);
                ?STATE_RUNNING ->
                    ok = after_process_2(SystemConf, MembersCur, MembersPrev)
            end;
        {{ok,_SystemConf}, {error,_Cause}} ->
            timer:apply_after(?CHECK_INTERVAL, ?MODULE,
                              inspect_cluster_status, [ok, Managers]);
        _Error ->
            timer:apply_after(?CHECK_INTERVAL, ?MODULE, inspect_cluster_status,
                              [ok, Managers]),
            io:format("~p:~s,~w - cause:~p~n", [?MODULE, "after_process/1", ?LINE,_Error])
    end,
    Res.


%% @private
is_alive_managers([]) ->
    false;
is_alive_managers([Manager|Rest]) ->
    case leo_misc:node_existence(Manager) of
        true ->            true;
        false ->
            is_alive_managers(Rest)
    end.


%%--------------------------------------------------------------------
%% Internal Functions.
%%--------------------------------------------------------------------
%% @doc After process of start_link
%% @private
-spec(after_process_0({ok, pid()} | {error, any()}) ->
             {ok, pid()} | {error, any()}).
after_process_0({ok, Pid}) ->
    ok = leo_misc:init_env(),
    Managers_0  = ?env_manager_nodes(leo_gateway),
    Managers_1 = lists:map(fun(X) when is_list(X) ->
                                   list_to_atom(X);
                              (X) ->
                                   X
                           end, Managers_0),

    case is_alive_managers(Managers_1) of
        true ->
            case catch after_process_1(Pid, Managers_1) of
                {ok, Pid} ->
                    {ok, Pid};
                {_, Cause} ->
                    ?error("after_process_0/1", [{cause, Cause}]),
                    init:stop()
            end;
        false ->
            ?error("inspect_cluster_status/1",
                   [{manager_nodes, Managers_1},
                    {cause, "Not alive managers"}]),
            init:stop()
    end;
after_process_0(Error) ->
    io:format("~p:~s,~w - cause:~p~n", [?MODULE, "after_process/1", ?LINE, Error]),
    init:stop().


%% @private
after_process_1(Pid, Managers) ->
    %% Launch leo_tran
    application:ensure_started(leo_tran),

    %% Launch SNMPA
    application:ensure_started(mnesia),
    application:ensure_started(snmp),
    ok = leo_statistics_api:start_link(leo_gateway),
    ok = leo_statistics_api:create_tables(ram_copies, [node()]),
    ok = leo_metrics_vm:start_link(?SNMP_SYNC_INTERVAL_10S),
    ok = leo_metrics_req:start_link(?SNMP_SYNC_INTERVAL_60S),
    ok = leo_gateway_cache_statistics:start_link(?SNMP_SYNC_INTERVAL_60S),

    %% Retrieve http-options
    {ok, HttpOptions} = get_options(),

    %% Launch bucket-sync, s3-related-procs
    %% [S3, NFS]
    Handler = HttpOptions#http_options.handler,
    case Handler of
        Handler when Handler == ?PROTO_HANDLER_S3;
                     Handler == ?PROTO_HANDLER_NFS ->
            %% Retrieve bucket-prop-sync-interval for S3-API
            BucketPropSyncInterval = ?env_bucket_prop_sync_interval(),

            %% Launch S3Libs:Auth/Bucket/EndPoint
            ok = leo_s3_libs:start(slave,
                                   [{'provider', Managers},
                                    {'bucket_prop_sync_interval', BucketPropSyncInterval}]),
            leo_s3_endpoint:get_endpoints();
        _ ->
            void
    end,

    %% Launch HTTP-handler
    %% [S3, REST]
    case Handler of
        Handler when Handler == ?PROTO_HANDLER_S3;
                     Handler == ?PROTO_HANDLER_REST ->
            %% Launch http-handler(s)
            ok = Handler:start(leo_gateway_sup, HttpOptions);
        _ ->
            void
    end,

    %% Launch nfs-related-procs
    %% [NFS]
    case Handler of
        ?PROTO_HANDLER_NFS ->
            %% NFS:Load nfs-rpc-server
            _ = application:load(nfs_rpc_server),

            NFS_Options = ?env_nfs_options(),
            MntdPort = leo_misc:get_value('mountd_port', NFS_Options, ?DEF_MOUNTD_PORT),
            MntdAcceptors = leo_misc:get_value('mountd_acceptors', NFS_Options, ?DEF_MOUNTD_ACCEPTORS),
            NFSdPort = leo_misc:get_value('nfsd_port', NFS_Options, ?DEF_NFSD_PORT),
            NFSdAcceptors = leo_misc:get_value('nfsd_acceptors', NFS_Options, ?DEF_NFSD_ACCEPTORS),
            LockdPort = leo_misc:get_value('lockd_port', NFS_Options, ?DEF_LOCKD_PORT),
            LockdAcceptors = leo_misc:get_value('lockd_acceptors', NFS_Options, ?DEF_LOCKD_ACCEPTORS),

            %% NFS:Argments for mountd
            MountdArgs = #nfs_rpc_app_arg{
                            ref = mountd3,
                            acceptor_num = MntdAcceptors,
                            trans_opts = [{port, MntdPort}],
                            prg_num = ?MOUNTPROG,
                            prg_name = mountprog,
                            prg_vsns = [],
                            vsn_lo = ?MOUNTVERS3,
                            vsn_hi = ?MOUNTVERS3,
                            use_pmap = true,
                            mod = leo_nfs_mount3_svc,
                            init_args = [],
                            state = []},

            %% NFS:Argments for nfsd
            NFS_D_Args = #nfs_rpc_app_arg{
                            ref = nfsd3,
                            acceptor_num = NFSdAcceptors,
                            trans_opts = [{port, NFSdPort}],
                            prg_num = ?NFS3_PROGRAM,
                            prg_name = nfs3_program,
                            prg_vsns = [],
                            vsn_lo = ?NFS_V3,
                            vsn_hi = ?NFS_V3,
                            use_pmap = true,
                            mod = leo_nfs_proto3_svc,
                            init_args = [],
                            state = []},

            %% NLM:Arguments for lockd
            LockdArgs = #nfs_rpc_app_arg{
                           ref = lockd4,
                           acceptor_num = LockdAcceptors,
                           trans_opts = [{port, LockdPort}],
                           prg_num = ?NLM_PROG,
                           prg_name = nlm_prog,
                           prg_vsns = [],
                           vsn_lo = ?NLM4_VERS,
                           vsn_hi = ?NLM4_VERS,
                           use_pmap = true,
                           mod = leo_nlm_proto4_svc,
                           init_args = [],
                           state = []},
            ok = application:set_env(nfs_rpc_server, args, [MountdArgs, NFS_D_Args, LockdArgs]),
            ok = application:ensure_started(nfs_rpc_server);
        _ ->
            void
    end,

    %% Launch LeoCache
    {ok, _} = supervisor:start_child(
                leo_gateway_sup, {leo_cache_sup,
                                  {leo_cache_sup, start_link,
                                   []}, permanent, 2000, worker, [leo_cache_sup]}),
    NumOfCacheWorkers     = HttpOptions#http_options.cache_workers,
    CacheRAMCapacity      = HttpOptions#http_options.cache_ram_capacity,
    CacheDiscCapacity     = HttpOptions#http_options.cache_disc_capacity,
    CacheDiscThresholdLen = HttpOptions#http_options.cache_disc_threshold_len,
    CacheDiscDirData      = HttpOptions#http_options.cache_disc_dir_data,
    CacheDiscDirJournal   = HttpOptions#http_options.cache_disc_dir_journal,
    ok = leo_cache_api:start([{?PROP_RAM_CACHE_NAME,           ?DEF_PROP_RAM_CACHE},
                              {?PROP_RAM_CACHE_WORKERS,        NumOfCacheWorkers},
                              {?PROP_RAM_CACHE_SIZE,           CacheRAMCapacity},
                              {?PROP_DISC_CACHE_NAME,          ?DEF_PROP_DISC_CACHE},
                              {?PROP_DISC_CACHE_WORKERS,       NumOfCacheWorkers},
                              {?PROP_DISC_CACHE_SIZE,          CacheDiscCapacity},
                              {?PROP_DISC_CACHE_THRESHOLD_LEN, CacheDiscThresholdLen},
                              {?PROP_DISC_CACHE_DATA_DIR,      CacheDiscDirData},
                              {?PROP_DISC_CACHE_JOURNAL_DIR,   CacheDiscDirJournal}
                             ]),

    %% Large Object Worker Pool
    ChildSpec_1  = {?POD_LOH_WORKER, {leo_pod_sup, start_link,
                                      [?POD_LOH_WORKER,
                                       ?env_loh_put_worker_pool_size(),
                                       ?env_loh_put_worker_buffer_size(),
                                       leo_large_object_worker, [],
                                       fun(_) ->
                                               void
                                       end]},
                    permanent, ?SHUTDOWN_WAITING_TIME,
                    supervisor, [leo_pod_sup]},
    {ok,_} = supervisor:start_child(leo_gateway_sup, ChildSpec_1),

    %% Launch SavannaAgent(QoS)
    SVManagers = ?env_qos_managers(),
    QoS_StatEnabled = ?env_qos_stat_enabled(),
    case QoS_StatEnabled of
        true ->
            %% launch savanna-agent and sync schema table
            ok = savanna_agent:start(ram_copies),
            ok = savanna_agent:sync_schemas(SVManagers);
        false ->
            void
    end,
    ChildSpec_2 = {leo_gateway_qos_stat,
                   {leo_gateway_qos_stat, start_link,
                    [SVManagers, QoS_StatEnabled]},
                   permanent, 2000, worker, [leo_gateway_qos_stat]},
    {ok,_} = supervisor:start_child(leo_gateway_sup, ChildSpec_2),

    %% Check status of the storage-cluster
    inspect_cluster_status({ok, Pid}, Managers).


%% @doc After process of start_link
%% @private
-spec(after_process_2(#?SYSTEM_CONF{}, list(#member{}), list(#member{})) ->
             ok).
after_process_2(SystemConf, MembersCur, MembersPrev) ->
    %% Launch Redundant-manager#2
    Managers    = ?env_manager_nodes(leo_gateway),
    NewManagers = lists:map(fun(X) when is_list(X) ->
                                    list_to_atom(X);
                               (X) ->
                                    X
                            end, Managers),
    case whereis(leo_redundant_manager_sup) of
        undefined ->
            ChildSpec = {leo_redundant_manager_sup,
                         {leo_redundant_manager_sup, start_link,
                          [?WORKER_NODE, NewManagers,
                           ?env_queue_dir(leo_gateway)]},
                         permanent, 2000, supervisor, [leo_redundant_manager_sup]},
            {ok, _} = supervisor:start_child(leo_gateway_sup, ChildSpec);
        _ ->
            {ok, _} = leo_redundant_manager_sup:start_link(
                        gateway, NewManagers, ?env_queue_dir(leo_gateway))
    end,
    ok = leo_redundant_manager_api:set_options(
           [{n, SystemConf#?SYSTEM_CONF.n},
            {r, SystemConf#?SYSTEM_CONF.r},
            {w, SystemConf#?SYSTEM_CONF.w},
            {d, SystemConf#?SYSTEM_CONF.d},
            {bit_of_ring, SystemConf#?SYSTEM_CONF.bit_of_ring},
            {num_of_dc_replicas,   SystemConf#?SYSTEM_CONF.num_of_dc_replicas},
            {num_of_rack_replicas, SystemConf#?SYSTEM_CONF.num_of_rack_replicas}]),

    {ok,_MembersChecksum} = leo_redundant_manager_api:synchronize(
                              ?SYNC_TARGET_MEMBER, [{?VER_CUR,  MembersCur },
                                                    {?VER_PREV, MembersPrev}]),
    {ok,_,_} = leo_redundant_manager_api:create(),
    ok = leo_membership_cluster_local:set_proc_auditor(leo_gateway_api),

    %% Register in THIS-Process
    ok = leo_gateway_api:register_in_monitor(first),
    lists:foldl(fun(N, false) ->
                        {ok, Checksums} = leo_redundant_manager_api:checksum(?CHECKSUM_RING),
                        case rpc:call(N, leo_manager_api, notify,
                                      [launched, gateway, node(), Checksums], ?DEF_TIMEOUT) of
                            ok -> true;
                            _  -> false
                        end;
                   (_, true) ->
                        void
                end, false, NewManagers),
    ok.


%% @doc Retrieve system-configuration from manager-node(s)
%% @private
-spec(get_system_config_from_manager(list()) ->
             {ok, #?SYSTEM_CONF{}} | {error, any()}).
get_system_config_from_manager([]) ->
    {error, 'could_not_get_system_config'};
get_system_config_from_manager([Manager|T]) ->
    case leo_misc:node_existence(Manager) of
        true ->
            case rpc:call(Manager, leo_manager_api, get_system_config, [], ?DEF_TIMEOUT) of
                {ok, SystemConf} ->
                    {ok, SystemConf};
                {badrpc, Why} ->
                    {error, Why};
                {error, Cause} ->
                    ?error("get_system_config_from_manager/1", [{cause, Cause}]),
                    get_system_config_from_manager(T)
            end;
        false ->
            get_system_config_from_manager(T)
    end.


%% @doc Retrieve members-list from manager-node(s)
%% @private
-spec(get_members_from_manager(list()) ->
             {ok, list()} | {error, any()}).
get_members_from_manager([]) ->
    {error, 'could_not_get_members'};
get_members_from_manager([Manager|T]) ->
    case rpc:call(Manager, leo_manager_api,
                  get_members_of_all_versions, [], ?DEF_TIMEOUT) of
        {ok, {MembersCur, MembersPrev}} ->
            {ok, {MembersCur, MembersPrev}};
        {badrpc, Why} ->
            {error, Why};
        {error, Cause} ->
            case Cause of
                not_found ->
                    void;
                _ ->
                    ?error("get_members_from_manager/1", [{cause, Cause}])
            end,
            get_members_from_manager(T)
    end.


%% @doc Retrieve the cluster status
%% @private
-spec(get_cluster_state(list(#member{})) ->
             node_state()).
get_cluster_state([]) ->
    ?STATE_STOP;
get_cluster_state([#member{state = ?STATE_RUNNING}|_]) ->
    ?STATE_RUNNING;
get_cluster_state([_|T]) ->
    get_cluster_state(T).


%% @doc Retrieve log-appneder(s)
%% @private
-spec(log_file_appender() ->
             list()).
log_file_appender() ->
    case application:get_env(leo_gateway, log_appender) of
        undefined   -> log_file_appender([], []);
        {ok, Value} -> log_file_appender(Value, [])
    end.

log_file_appender([], []) ->
    [{?LOG_ID_FILE_INFO,  ?LOG_APPENDER_FILE},
     {?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}];
log_file_appender([], Acc) ->
    lists:reverse(Acc);
log_file_appender([{Type, _}|T], Acc) when Type == file ->
    log_file_appender(T, [{?LOG_ID_FILE_ERROR, ?LOG_APPENDER_FILE}|
                          [{?LOG_ID_FILE_INFO, ?LOG_APPENDER_FILE}|Acc]]).


%% @doc Retrieve properties
%%
-spec(get_options() ->
             {ok, #http_options{}} | {error, Cause} when Cause::any()).
get_options() ->
    HttpProp = ?env_http_properties(),

    %% Retrieve ptotocol:
    Protocol = case ?env_protocol() of
                   [] ->
                       leo_misc:get_value('handler', HttpProp,
                                          ?DEF_PROTOCOL_HANDLER);
                   V ->
                       V
               end,

    %% Retrieve http-related properties:
    Port = leo_misc:get_value('port', HttpProp, ?DEF_HTTP_PORT),
    SSLPort = leo_misc:get_value('ssl_port', HttpProp, ?DEF_HTTP_SSL_PORT),
    SSLCertFile = leo_misc:get_value('ssl_certfile', HttpProp, ?DEF_HTTP_SSL_C_FILE),
    SSLKeyFile = leo_misc:get_value('ssl_keyfile', HttpProp, ?DEF_HTTP_SSL_K_FILE),
    NumOfAcceptors = leo_misc:get_value('num_of_acceptors', HttpProp, ?DEF_HTTP_NUM_OF_ACCEPTORS),
    MaxKeepAlive = leo_misc:get_value('max_keepalive', HttpProp, ?DEF_HTTP_MAX_KEEPALIVE),
    CustomHeaderConf = leo_misc:get_value('headers_config_file', HttpProp, ?DEF_HTTP_CUSTOM_HEADER_CONF),
    Timeout4Header = leo_misc:get_value('timeout_for_header', HttpProp, ?DEF_HTTP_TIMEOUT_FOR_HEADER),
    Timeout4Body = leo_misc:get_value('timeout_for_body', HttpProp, ?DEF_HTTP_TIMEOUT_FOR_BODY),
    SendChunkLen = leo_misc:get_value('sending_chunked_obj_len', HttpProp, ?DEF_HTTP_SEND_CHUNK_LEN),

    %% Retrieve cache-related properties:
    CacheProp = ?env_cache_properties(),
    UserHttpCache = leo_misc:get_value('http_cache', CacheProp, ?DEF_HTTP_CACHE),
    CacheWorkers = leo_misc:get_value('cache_workers', CacheProp, ?DEF_CACHE_WORKERS),

    CacheRAMCapacity =
        case leo_misc:get_value('cache_ram_capacity', CacheProp, ?DEF_CACHE_RAM_CAPACITY) of
            {error, Cause_1} ->
                erlang:error(Cause_1);
            CacheRAMCapacity_1 ->
                CacheRAMCapacity_1
        end,
    CacheDiscCapacity =
        case leo_misc:get_value('cache_disc_capacity', CacheProp, ?DEF_CACHE_DISC_CAPACITY) of
            {error, Cause_2} ->
                erlang:error(Cause_2);
            CacheDiscCapacity_1 ->
                CacheDiscCapacity_1
        end,

    CacheDiscThresholdLen = leo_misc:get_value('cache_disc_threshold_len', CacheProp, ?DEF_CACHE_DISC_THRESHOLD_LEN),
    CacheDiscDirData = leo_misc:get_value('cache_disc_dir_data', CacheProp, ?DEF_CACHE_DISC_DIR_DATA),
    CacheDiscDirJournal = leo_misc:get_value('cache_disc_dir_journal', CacheProp, ?DEF_CACHE_DISC_DIR_JOURNAL),
    CacheExpire = leo_misc:get_value('cache_expire', CacheProp, ?DEF_CACHE_EXPIRE),
    CacheMaxContentLen = leo_misc:get_value('cache_max_content_len', CacheProp, ?DEF_CACHE_MAX_CONTENT_LEN),
    CachableContentTypes = leo_misc:get_value('cachable_content_type', CacheProp, []),
    CachablePathPatterns = leo_misc:get_value('cachable_path_pattern', CacheProp, []),

    CacheMethod = case UserHttpCache of
                      true  -> ?CACHE_HTTP;
                      false -> ?CACHE_INNER
                  end,
    CachableContentTypes1 = cast_type_list_to_binary(CachableContentTypes),
    CachablePathPatterns1 = case cast_type_list_to_binary(CachablePathPatterns) of
                                [] -> [];
                                List ->
                                    lists:foldl(
                                      fun(P, Acc) ->
                                              case re:compile(P) of
                                                  {ok, MP} -> [MP|Acc];
                                                  _        -> Acc
                                              end
                                      end, [], List)
                            end,

    %% Retrieve large-object-related properties:
    LargeObjectProp = ?env_large_object_properties(),
    MaxChunkedObjs = leo_misc:get_value('max_chunked_objs', LargeObjectProp, ?DEF_LOBJ_MAX_CHUNKED_OBJS),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len', LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    ReadingChunkedLen = leo_misc:get_value('reading_chunked_obj_len', LargeObjectProp, ?DEF_LOBJ_READING_CHUNK_OBJ_LEN),
    case ReadingChunkedLen of
        {error, Cause} ->
            %% stop to handle
            %% https://github.com/leo-project/leofs/issues/531
            error({error, Cause});
        _ ->
            nop
    end,
    ThresholdChunkLen = leo_misc:get_value('threshold_of_chunk_len', LargeObjectProp, ?DEF_LOBJ_THRESHOLD_OF_CHUNK_LEN),
    MaxObjLen = MaxChunkedObjs * ChunkedObjLen,

    %% Retrieve timeout-values
    lists:foreach(fun({K, T}) ->
                          leo_misc:set_env(leo_gateway, K, T)
                  end, ?env_timeout()),
    HttpOptions = #http_options{handler = ?convert_to_handler(Protocol),
                                port = Port,
                                ssl_port = SSLPort,
                                ssl_certfile = SSLCertFile,
                                ssl_keyfile = SSLKeyFile,
                                num_of_acceptors = NumOfAcceptors,
                                max_keepalive = MaxKeepAlive,
                                headers_config_file = CustomHeaderConf,
                                timeout_for_header = Timeout4Header,
                                timeout_for_body = Timeout4Body,
                                sending_chunked_obj_len = SendChunkLen,
                                cache_method = CacheMethod,
                                cache_workers = CacheWorkers,
                                cache_ram_capacity = CacheRAMCapacity,
                                cache_disc_capacity = CacheDiscCapacity,
                                cache_disc_threshold_len = CacheDiscThresholdLen,
                                cache_disc_dir_data = CacheDiscDirData,
                                cache_disc_dir_journal = CacheDiscDirJournal,
                                cache_expire = CacheExpire,
                                cache_max_content_len = CacheMaxContentLen,
                                cachable_content_type = CachableContentTypes1,
                                cachable_path_pattern = CachablePathPatterns1,
                                max_chunked_objs = MaxChunkedObjs,
                                max_len_of_obj = MaxObjLen,
                                chunked_obj_len = ChunkedObjLen,
                                reading_chunked_obj_len = ReadingChunkedLen,
                                threshold_of_chunk_len = ThresholdChunkLen},
    ?info("start/3", "protocol: ~p", [Protocol]),
    ?info("start/3", "port: ~p", [Port]),
    ?info("start/3", "ssl port: ~p", [SSLPort]),
    ?info("start/3", "ssl certfile: ~p", [SSLCertFile]),
    ?info("start/3", "ssl keyfile: ~p", [SSLKeyFile]),
    ?info("start/3", "num of acceptors: ~p", [NumOfAcceptors]),
    ?info("start/3", "max keepalive: ~p", [MaxKeepAlive]),
    ?info("start/3", "http custom header config: ~p",[CustomHeaderConf]),
    ?info("start/3", "timeout for header : ~p", [Timeout4Header]),
    ?info("start/3", "timeout for body: ~p", [Timeout4Body]),
    ?info("start/3", "sending chunk length: ~p", [SendChunkLen]),
    ?info("start/3", "cache_method: ~p", [CacheMethod]),
    ?info("start/3", "cache workers: ~p", [CacheWorkers]),
    ?info("start/3", "cache ram capacity: ~p", [CacheRAMCapacity]),
    ?info("start/3", "cache disc capacity: ~p", [CacheDiscCapacity]),
    ?info("start/3", "cache disc threshold len: ~p", [CacheDiscThresholdLen]),
    ?info("start/3", "cache disc data-dir: ~p", [CacheDiscDirData]),
    ?info("start/3", "cache disc journal-dir: ~p", [CacheDiscDirJournal]),
    ?info("start/3", "cache expire: ~p", [CacheExpire]),
    ?info("start/3", "cache_max_content_len: ~p", [CacheMaxContentLen]),
    ?info("start/3", "cacheable_content_types: ~p", [CachableContentTypes]),
    ?info("start/3", "cacheable_path_patterns: ~p", [CachablePathPatterns]),
    ?info("start/3", "max_chunked_obj: ~p", [MaxChunkedObjs]),
    ?info("start/3", "max_len_of_obj: ~p", [MaxObjLen]),
    ?info("start/3", "chunked_obj_len: ~p", [ChunkedObjLen]),
    ?info("start/3", "reading_chunked_obj_len: ~p", [ReadingChunkedLen]),
    ?info("start/3", "threshold_of_chunk_len: ~p", [ThresholdChunkLen]),
    {ok, HttpOptions}.


%% @doc Data-type transmit from list to binary
%% @private
cast_type_list_to_binary([]) ->
    [];
cast_type_list_to_binary(List) ->
    lists:map(fun(I) ->
                      case catch list_to_binary(I) of
                          {'EXIT', _} -> I;
                          Bin         -> Bin
                      end
              end, List).
