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
%% -------------------------------------------------------------------
%% Leo Gateway - API
%% @doc
%% @end
%%====================================================================
-module(leo_gateway_api).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_watchdog/include/leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([get_node_status/0,
         register_in_monitor/1, register_in_monitor/2,
         purge/1, update_manager_nodes/1,
         update_conf/2
        ]).

-export([set_endpoint/1, delete_endpoint/1,
         update_acl/3]).

-export([add_bucket/4, delete_bucket/3, update_bucket/1]).


%%----------------------------------------------------------------------
%% API-Function(s)
%%----------------------------------------------------------------------
%% @doc Get node status (gateway).
%%
-spec(get_node_status() -> {ok, list()}).
get_node_status() ->
    Statistics = [{vm_version, erlang:system_info(version)},
                  {total_mem_usage, erlang:memory(total)},
                  {system_mem_usage, erlang:memory(system)},
                  {proc_mem_usage, erlang:memory(processes)},
                  {ets_mem_usage, erlang:memory(ets)},
                  {num_of_procs, erlang:system_info(process_count)},
                  {process_limit, erlang:system_info(process_limit)},
                  {kernel_poll, erlang:system_info(kernel_poll)},
                  {thread_pool_size, erlang:system_info(thread_pool_size)}
                 ],
    Protocol = ?env_protocol(),
    HttpProps = ?env_http_properties(),
    CacheProps = ?env_cache_properties(),
    LObjProps = ?env_large_object_properties(),
    HttpConf =
        [
         %% Protocol-related
         {handler, leo_misc:get_value('protocol', HttpProps, Protocol)},
         {port, leo_misc:get_value('port', HttpProps, ?DEF_HTTP_PORT)},
         {ssl_port, leo_misc:get_value('ssl_port', HttpProps, ?DEF_HTTP_SSL_PORT)},
         {num_of_acceptors, leo_misc:get_value('num_of_acceptors', HttpProps, ?DEF_HTTP_NUM_OF_ACCEPTORS)},
         %% Cache-related
         {http_cache, leo_misc:get_value('http_cache', CacheProps, ?DEF_HTTP_CACHE)},
         {cache_workers, leo_misc:get_value('cache_workers', CacheProps, ?DEF_CACHE_WORKERS)},
         {cache_ram_capacity, leo_misc:get_value('cache_ram_capacity', CacheProps, ?DEF_CACHE_RAM_CAPACITY)},
         {cache_disc_capacity, leo_misc:get_value('cache_disc_capacity', CacheProps, ?DEF_CACHE_DISC_CAPACITY)},
         {cache_disc_threshold_len, leo_misc:get_value('cache_disc_threshold_len', CacheProps, ?DEF_CACHE_DISC_THRESHOLD_LEN)},
         {cache_disc_dir_data, leo_misc:get_value('cache_disc_dir_data', CacheProps, ?DEF_CACHE_DISC_DIR_DATA)},
         {cache_disc_dir_journal, leo_misc:get_value('cache_disc_dir_journal', CacheProps, ?DEF_CACHE_DISC_DIR_JOURNAL)},
         {cache_expire, leo_misc:get_value('cache_expire', CacheProps, ?DEF_CACHE_EXPIRE)},
         {cache_max_content_len, leo_misc:get_value('cache_max_content_len', CacheProps, ?DEF_CACHE_MAX_CONTENT_LEN)},
         {cachable_content_type, leo_misc:get_value('cachable_content_type', CacheProps, [])},
         {cachable_path_pattern, leo_misc:get_value('cachable_path_pattern', CacheProps, [])},
         %% LargeObject-related
         {max_chunked_objs, leo_misc:get_value('max_chunked_objs', LObjProps, ?DEF_LOBJ_MAX_CHUNKED_OBJS)},
         {chunked_obj_len, leo_misc:get_value('chunked_obj_len', LObjProps, ?DEF_LOBJ_CHUNK_OBJ_LEN)},
         {reading_chunked_obj_len, leo_misc:get_value('reading_chunked_obj_len', LObjProps, ?DEF_LOBJ_READING_CHUNK_OBJ_LEN)},
         {threshold_of_chunk_len, leo_misc:get_value('threshold_of_chunk_len', LObjProps, ?DEF_LOBJ_THRESHOLD_OF_CHUNK_LEN)},
         {loh_put_worker_pool_size, leo_misc:get_value('put_worker_pool_size', LObjProps, ?DEF_LOH_PUT_WORKER_POOL_SIZE)},
         {loh_put_worker_buffer_size, leo_misc:get_value('put_worker_buffer_size', LObjProps, ?DEF_LOH_PUT_WORKER_BUFFER_SIZE)}
        ],
    {ok, [{type, gateway},
          {version, get_info(version)},
          {dirs, get_info(directories)},
          {ring_checksum, get_info(ring_hashes)},
          {watchdog,
           [{cpu_enabled, ?env_wd_cpu_enabled()},
            {io_enabled, ?env_wd_io_enabled()},
            {rex_interval, ?env_wd_rex_interval()},
            {cpu_interval, ?env_wd_cpu_interval()},
            {io_interval, ?env_wd_io_interval()},
            {rex_threshold_mem_capacity, ?env_wd_threshold_mem_capacity()},
            {cpu_threshold_cpu_load_avg, ?env_wd_threshold_cpu_load_avg()},
            {cpu_threshold_cpu_util, ?env_wd_threshold_cpu_util()},
            {cpu_raised_error_times, ?env_wd_cpu_raised_error_times()},
            {io_threshold_input_per_sec, ?env_wd_threshold_input_per_sec()},
            {io_threshold_output_per_sec, ?env_wd_threshold_output_per_sec()}
           ]
          },
          {log_level, get_info(log_level)},
          {statistics, Statistics},
          {http_conf, HttpConf}
         ]}.

%% Retrieve the environement values
%% @private
get_info(version) ->
    case application:get_key(leo_gateway, vsn) of
        {ok, Ver} ->
            Ver;
        _ ->
            "undefined"
    end;
get_info(ring_hashes) ->
    {RingHashCur, RingHashPrev} =
        case leo_redundant_manager_api:checksum(?CHECKSUM_RING) of
            {ok, {Chksum0, Chksum1}} ->
                {Chksum0, Chksum1};
            _ ->
                {[],[]}
        end,
    [{ring_cur, RingHashCur},
     {ring_prev, RingHashPrev }
    ];
get_info(directories) ->
    SNMPAgent =
        case application:get_env(leo_gateway, snmp_agent) of
            {ok, EnvSNMPAgent} ->
                EnvSNMPAgent;
            _ ->
                []
        end,
    [{log, ?env_log_dir(leo_gateway)},
     {mnesia, []},
     {snmp_agent, SNMPAgent}
    ];
get_info(log_level) ->
    case application:get_env(leo_gateway, log_level) of
        {ok, 0} ->
            "debug";
        {ok, 1} ->
            "info";
        {ok, 2} ->
            "warn";
        {ok, 3} ->
            "error";
        {ok, 4} ->
            "fatal";
        _ ->
            "undefined"
    end;
get_info(_) ->
    [].


%% @doc Register into the manager-monitor.
%%
-spec(register_in_monitor(first|again) ->
             ok).
register_in_monitor(RequestedTimes) ->
    case whereis(leo_gateway_sup) of
        undefined ->
            {error, not_found};
        Pid ->
            register_in_monitor(Pid, RequestedTimes)
    end.

register_in_monitor(Pid, RequestedTimes) ->
    ManagerNodes = ?env_manager_nodes(leo_gateway),
    register_in_monitor(ManagerNodes, Pid, RequestedTimes).

register_in_monitor([],_,_) ->
    {error, ?ERROR_COULD_NOT_CONNECT};
register_in_monitor([Node1|Rest], Pid, RequestedTimes) ->
    Node2 = case is_list(Node1) of
                true  -> list_to_atom(Node1);
                false -> Node1
            end,
    Ret = case leo_misc:node_existence(Node2) of
              true ->
                  case rpc:call(Node2, leo_manager_api, register,
                                [RequestedTimes, Pid,
                                 erlang:node(), ?WORKER_NODE], ?DEF_TIMEOUT) of
                      {ok, SystemConf} ->
                          Options = lists:zip(
                                      record_info(
                                        fields, ?SYSTEM_CONF),
                                      tl(tuple_to_list(SystemConf))),
                          ok = leo_redundant_manager_api:set_options(Options),
                          true;
                      {error, Cause} ->
                          ?warn("register_in_monitor/3",
                                [{manager, Node2}, {cause, Cause}]),
                          false;
                      {badrpc, Cause} ->
                          ?warn("register_in_monitor/3",
                                [{manager, Node2}, {cause, Cause}]),
                          false
                  end;
              false ->
                  false
          end,
    case Ret of
        true ->
            ok;
        false ->
            register_in_monitor(Rest, Pid, RequestedTimes)
    end.


%% @doc Purge an object into the cache
-spec(purge(string()) -> ok).
purge(Path) ->
    BinPath = list_to_binary(Path),
    catch leo_cache_api:delete(BinPath),
    ok.


%% @doc update manager nodes
%%
-spec(update_manager_nodes(list()) ->
             ok).
update_manager_nodes(Managers) ->
    ?update_env_manager_nodes(leo_gateway, Managers),
    ok = leo_membership_cluster_local:update_manager_nodes(Managers),
    leo_s3_libs:update_providers(Managers).


%% @doc Update a configuration dinamically
-spec(update_conf(Key, Val) ->
             ok | {error, any()} when Key::atom(),
                                      Val::any()).
update_conf(log_level, Val) when Val == ?LOG_LEVEL_DEBUG;
                                 Val == ?LOG_LEVEL_INFO;
                                 Val == ?LOG_LEVEL_WARN;
                                 Val == ?LOG_LEVEL_ERROR;
                                 Val == ?LOG_LEVEL_FATAL ->
    case application:set_env(leo_gateway, log_level, Val) of
        ok ->
            leo_logger_client_message:update_log_level(Val);
        _ ->
            {error, ?ERROR_COULD_NOT_UPDATE_LOG_LEVEL}
    end;
update_conf(consistency_level, {W, R, D}) when is_integer(W),
                                               is_integer(R),
                                               is_integer(D) ->
    {ok, RedConf} = leo_redundant_manager_api:get_options(),
    RedConf_1 = case leo_misc:get_value(?PROP_W, RedConf) of
                    undefined ->
                        [{?PROP_W, W}|RedConf];
                    Item_W ->
                        [{?PROP_W, W}|lists:delete({?PROP_W, Item_W}, RedConf)]
                end,
    RedConf_2 = case leo_misc:get_value(?PROP_R, RedConf_1) of
                    undefined ->
                        [{?PROP_R, R}|RedConf_1];
                    Item_R ->
                        [{?PROP_R, R}|lists:delete({?PROP_R, Item_R}, RedConf_1)]
                end,
    RedConf_3 = case leo_misc:get_value(?PROP_D, RedConf_2) of
                    undefined ->
                        [{?PROP_D, D}|RedConf_2];
                    Item_D ->
                        [{?PROP_D, D}|lists:delete({?PROP_D, Item_D}, RedConf_2)]
                end,
    leo_redundant_manager_api:set_options(RedConf_3);
update_conf(_,_) ->
    {error, badarg}.


%%----------------------------------------------------------------------
%% S3API-related Function(s)
%%----------------------------------------------------------------------
%% @doc Set s3-endpoint from manager (S3-API)
%%
-spec(set_endpoint(binary()) ->
             ok | {error, any()}).
set_endpoint(Endpoint) ->
    leo_s3_endpoint:set_endpoint(Endpoint).


%% @doc Set s3-endpoint from manager (S3-API)
%%
-spec(delete_endpoint(binary()) ->
             ok | {error, any()}).
delete_endpoint(Endpoint) ->
    leo_s3_endpoint:delete_endpoint(Endpoint).


%% @doc Update permission by access-key-id (S3-API)
%%
-spec(update_acl(string(), binary(), binary()) ->
             ok | {error, any()}).
update_acl(?CANNED_ACL_PRIVATE, AccessKey, Bucket) ->
    leo_s3_bucket:update_acls2private(AccessKey, Bucket);
update_acl(?CANNED_ACL_PUBLIC_READ, AccessKey, Bucket) ->
    leo_s3_bucket:update_acls2public_read(AccessKey, Bucket);
update_acl(?CANNED_ACL_PUBLIC_READ_WRITE, AccessKey, Bucket) ->
    leo_s3_bucket:update_acls2public_read_write(AccessKey, Bucket);
update_acl(?CANNED_ACL_AUTHENTICATED_READ, AccessKey, Bucket) ->
    leo_s3_bucket:update_acls2authenticated_read(AccessKey, Bucket);
update_acl(_,_,_) ->
    {error, invalid_args}.


%% @doc Add a bucket(S3-API)
%%
-spec(add_bucket(binary(), binary(), string(), atom()) ->
             ok | {error, any()}).
add_bucket(AccessKey, Bucket, CannedACL, _Atom) ->
    leo_s3_bucket:put(AccessKey, Bucket, CannedACL, _Atom, ets).


%% @doc Delete a bucket(S3-API)
%%
-spec(delete_bucket(binary(), binary(), atom()) ->
             ok | {error, any()}).
delete_bucket(AccessKey, Bucket, _Atom) ->
    leo_s3_bucket:delete(AccessKey, Bucket, _Atom).


%% @doc Update a bucket(S3-API)
%%
-spec(update_bucket(Bucket) ->
             ok | {error, any()} when Bucket::#?BUCKET{}).
update_bucket(Bucket) ->
    leo_s3_bucket:put(Bucket).
