%%======================================================================
%%
%% LeoManager
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
-module(leo_manager_formatter_text).

-include("leo_manager.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_user.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ok/0, error/1, error/2, help/1, version/1,
         bad_nodes/1, system_info_and_nodes_stat/1, node_stat/2,
         compact_status/1, du/2,
         mq_stats/1,
         credential/2, users/1, endpoints/1,
         buckets/1, bucket_by_access_key/1,
         del_bucket_stats/1, del_bucket_stats_all/1,
         acls/1, cluster_status/1,
         whereis/1, nfs_mnt_key/1,
         histories/1,
         version_all/1,
         authorized/0, user_id/0, password/0
        ]).


%% @doc Format 'ok'
-spec(ok() ->
             string()).
ok() ->
    ?OK.

%% @doc Format 'error'
-spec(error(string()) ->
             string()).
error(Cause) when is_list(Cause) ->
    io_lib:format("[ERROR] ~s\r\n", [Cause]);
error(Cause) ->
    io_lib:format("[ERROR] ~p\r\n", [Cause]).

-spec(error(atom() | string(), string()) ->
             string()).
error(Node, Cause) when is_atom(Node) ->
    io_lib:format("[ERROR] node:~w, ~s\r\n", [Node, Cause]);
error(Node, Cause) ->
    io_lib:format("[ERROR] node:~s, ~s\r\n", [Node, Cause]).


%% @doc Format 'help'
-spec(help(undefined | atom()) ->
             string()).
help(PluginMod) ->
    Help_1 = lists:append([help("[Cluster Operation]\r\n",
                                [?CMD_DETACH,
                                 ?CMD_SUSPEND,
                                 ?CMD_RESUME,
                                 ?CMD_ROLLBACK,
                                 ?CMD_START,
                                 ?CMD_REBALANCE,
                                 ?CMD_WHEREIS,
                                 ?CMD_RECOVER
                                ], []),
                           help("[MQ-related commands]\r\n",
                                [?CMD_MQ_STATS,
                                 ?CMD_MQ_SUSPEND,
                                 ?CMD_MQ_RESUME
                                ], []),
                           help("[Storage Maintenance]\r\n",
                                [?CMD_DU,
                                 ?CMD_COMPACT,
                                 ?CMD_RECOVER
                                ], []),
                           help("[Gateway Maintenance]\r\n",
                                [?CMD_PURGE,
                                 ?CMD_REMOVE
                                ], []),
                           help("[Watchdog Operation]\r\n",
                                [?CMD_UPDATE_PROP
                                ], []),
                           help("[Manager Maintenance]\r\n",
                                [?CMD_UPDATE_MANAGERS,
                                 ?CMD_BACKUP_MNESIA,
                                 ?CMD_RESTORE_MNESIA,
                                 ?CMD_DUMP_RING,
                                 ?CMD_UPDATE_LOG_LEVEL,
                                 ?CMD_UPDATE_CONSISTENCY_LEVEL
                                ], []),
                           help("[S3-related Maintenance]\r\n",
                                [?CMD_CREATE_USER,
                                 ?CMD_IMPORT_USER,
                                 ?CMD_DELETE_USER,
                                 ?CMD_UPDATE_USER_ROLE,
                                 ?CMD_UPDATE_USER_PW,
                                 ?CMD_GET_USERS,
                                 ?CMD_ADD_ENDPOINT,
                                 ?CMD_DEL_ENDPOINT,
                                 ?CMD_GET_ENDPOINTS,
                                 ?CMD_ADD_BUCKET,
                                 ?CMD_DELETE_BUCKET,
                                 ?CMD_DELETE_BUCKET_STATS,
                                 ?CMD_GET_BUCKETS,
                                 ?CMD_GET_BUCKET_BY_ACCESS_KEY,
                                 ?CMD_CHANGE_BUCKET_OWNER,
                                 ?CMD_SET_RED_METHOD,
                                 ?CMD_UPDATE_ACL], []),
                           help("[Multi-DC Replication]\r\n",
                                [?CMD_JOIN_CLUSTER,
                                 ?CMD_REMOVE_CLUSTER,
                                 ?CMD_CLUSTER_STAT
                                ], [])]),
    Help_2 = case PluginMod of
                 undefined -> [];
                 _ ->
                     PluginMod:help()
             end,
    Help_3 = lists:append([help("[Misc]\r\n",
                                [?CMD_VERSION,
                                 ?CMD_STATUS,
                                 ?CMD_DUMP_RING,
                                 ?CMD_QUIT], [])]),
    lists:append([Help_1, Help_2, Help_3]).


%% @doc Output the usage
%% @private
help(_, [], []) ->
    [];
help(Header, [], Acc) ->
    lists:append([[Header], Acc, [?CRLF]]);
help(Header, [Command|Rest], Acc) ->
    case get_formatted_help(Command) of
        [] ->
            help(Header, Rest, Acc);
        Res ->
            Acc1 = lists:append([Acc, [Res]]),
            help(Header, Rest, Acc1)
    end.

%% @private
get_formatted_help(Command) ->
    case leo_manager_mnesia:get_available_command_by_name(Command) of
        {ok, [#cmd_state{help = Help}|_]} ->
            io_lib:format("~s\r\n", [Help]);
        _ ->
            []
    end.


%% @doc Format 'version'
-spec(version(string()) ->
             string()).
version(Version) ->
    lists:append([Version, ?CRLF]).


%% @doc Format 'bad nodes'
-spec(bad_nodes(list()) ->
             string()).
bad_nodes(BadNodes) ->
    lists:foldl(fun(Node, Acc) ->
                        Acc ++ io_lib:format("[ERROR] ~w\r\n", [Node])
                end, [], BadNodes).


%% @doc Format the resulst of a list of cluster-nodes
-spec(system_info_and_nodes_stat(list()) ->
             string()).
system_info_and_nodes_stat(Props) ->
    SystemConf = leo_misc:get_value('system_config', Props),
    Version    = leo_misc:get_value('version',       Props),
    [RH0, RH1] = leo_misc:get_value('ring_hash',     Props),
    Nodes      = leo_misc:get_value('nodes',         Props),

    %% Output format:
    %% [System config]
    %%                 System version : 1.0.0
    %%                     Cluster Id : leofs-1
    %%                          DC Id : dc-1
    %%                 Total replicas : 3
    %%            # of successes of R : 1
    %%            # of successes of W : 2
    %%            # of successes of D : 1
    %%  # of Rack-awareness replicas  : 0
    %%                      ring size : 2^128
    %%              Current ring hash : 41e0c107
    %%                 Prev ring hash : 41e0c107
    %% [Multi DC replication settings]
    %%        # of destination of DCs : 0
    %%        # of replicas to a DC   : 0
    %%       mdcr/# of successes of R : 1
    %%       mdcr/# of successes of W : 2
    %%       mdcr/# of successes of D : 1
    FormattedSystemConf =
        io_lib:format(lists:append([
                                    " [System Confiuration]\r\n",
                                    "-----------------------------------+----------\r\n",
                                    " Item                              | Value    \r\n",
                                    "-----------------------------------+----------\r\n",
                                    " Basic/Consistency level\r\n",
                                    "-----------------------------------+----------\r\n",
                                    "                    system version | ~s\r\n",
                                    "                        cluster Id | ~s\r\n",
                                    "                             DC Id | ~s\r\n",
                                    "                    Total replicas | ~w\r\n",
                                    "          number of successes of R | ~w\r\n",
                                    "          number of successes of W | ~w\r\n",
                                    "          number of successes of D | ~w\r\n",
                                    " number of rack-awareness replicas | ~w\r\n",
                                    "                         ring size | 2^~w\r\n",
                                    "-----------------------------------+----------\r\n",
                                    " Multi DC replication settings\r\n",
                                    "-----------------------------------+----------\r\n",
                                    " [mdcr] max number of joinable DCs | ~w\r\n",
                                    " [mdcr] total replicas per a DC    | ~w\r\n",
                                    " [mdcr] number of successes of R   | ~w\r\n",
                                    " [mdcr] number of successes of W   | ~w\r\n",
                                    " [mdcr] number of successes of D   | ~w\r\n",
                                    "-----------------------------------+----------\r\n",
                                    " Manager RING hash\r\n",
                                    "-----------------------------------+----------\r\n",
                                    "                 current ring-hash | ~s\r\n",
                                    "                previous ring-hash | ~s\r\n",
                                    "-----------------------------------+----------\r\n\r\n"
                                   ]),
                      [Version,
                       SystemConf#?SYSTEM_CONF.cluster_id,
                       SystemConf#?SYSTEM_CONF.dc_id,
                       SystemConf#?SYSTEM_CONF.n,
                       SystemConf#?SYSTEM_CONF.r,
                       SystemConf#?SYSTEM_CONF.w,
                       SystemConf#?SYSTEM_CONF.d,
                       SystemConf#?SYSTEM_CONF.num_of_rack_replicas,
                       SystemConf#?SYSTEM_CONF.bit_of_ring,
                       SystemConf#?SYSTEM_CONF.max_mdc_targets,
                       SystemConf#?SYSTEM_CONF.num_of_dc_replicas,
                       SystemConf#?SYSTEM_CONF.mdcr_r,
                       SystemConf#?SYSTEM_CONF.mdcr_w,
                       SystemConf#?SYSTEM_CONF.mdcr_d,
                       case (RH0 < 1) of
                           true ->
                               [];
                           false ->
                               leo_hex:integer_to_hex(RH0, 8)
                       end,
                       case (RH1 < 1) of
                           true ->
                               [];
                           false ->
                               leo_hex:integer_to_hex(RH1, 8)
                       end
                      ]),
    system_conf_with_node_stat(FormattedSystemConf, Nodes).


%% @doc Format the result of a system-configuration w/node-state
-spec(system_conf_with_node_stat(string(), list()) ->
             string()).
system_conf_with_node_stat(FormattedSystemConf, []) ->
    FormattedSystemConf;
system_conf_with_node_stat(FormattedSystemConf, Nodes) ->
    Col_1_Len = lists:foldl(fun({_,N,_,_,_,_}, Acc) ->
                                    Len = length(N),
                                    case (Len > Acc) of
                                        true  -> Len;
                                        false -> Acc
                                    end
                            end, 0, Nodes) + 5,
    CellColumns = [{"type", 6},
                   {"node", Col_1_Len},
                   {"state", 12},
                   {"current ring", 14},
                   {"prev ring", 14},
                   {"updated at", 28},
                   {'$end', 0}],
    LenPerCol = lists:map(fun({_, Len}) -> Len end, CellColumns),


    Fun1 = fun(Col, Str) ->
                   case Col of
                       {'$end',_Len      } -> lists:append([Str, ?CRLF]);
                       {"type", Len      } -> lists:append([Str, lists:duplicate(Len + 1, "-"), "+"]);
                       {"node", Len      } -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"]);
                       {"updated at", Len} -> lists:append([Str, lists:duplicate(Len + 0, "-")]);
                       {_Other, Len      } -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"])
                   end
           end,
    Header1 = lists:foldl(Fun1, [], CellColumns),

    Fun2 = fun(Col, Str) ->
                   {Name, _} = Col,
                   case Col of
                       {'$end',_Len      } -> lists:append([Str, ?CRLF]);
                       {"updated at", Len} -> lists:append([Str, string:centre(Name, Len, $ )]);
                       {_Other, Len      } -> lists:append([Str, string:centre(Name, Len, $ ), ?SEPARATOR])
                   end
           end,
    Header2 = lists:foldl(Fun2, [], CellColumns),

    Fun3 = fun(N, List) ->
                   {Type, Alias, State, RingHash0, RingHash1, When} = N,
                   FormattedDate = leo_date:date_format(When),
                   Ret = lists:append([string:centre(Type,    lists:nth(1,LenPerCol)), ?SEPARATOR,
                                       string:left(Alias,     lists:nth(2,LenPerCol)), ?SEPARATOR,
                                       string:left(State,     lists:nth(3,LenPerCol)), ?SEPARATOR,
                                       string:left(RingHash0, lists:nth(4,LenPerCol)), ?SEPARATOR,
                                       string:left(RingHash1, lists:nth(5,LenPerCol)), ?SEPARATOR,
                                       FormattedDate,
                                       ?CRLF]),
                   List ++ [Ret]
           end,
    lists:foldl(Fun3, [FormattedSystemConf,
                       " [State of Node(s)]\r\n",
                       Header1,Header2, Header1], Nodes) ++ Header1 ++ ?CRLF.

%% @doc Format the resylt of a list of versions
-spec(version_all(list()) ->
             string()).
version_all(Nodes) ->
    Col_1_Len = lists:foldl(fun({_,N,_}, Acc) ->
                                    Len = length(N),
                                    case (Len > Acc) of
                                        true  -> Len;
                                        false -> Acc
                                    end
                            end, 0, Nodes) + 5,
    CellColumns = [{"type", 6},
                   {"node", Col_1_Len},
                   {"version", 32},
                   {'$end', 0}],
    LenPerCol = lists:map(fun({_, Len}) -> Len end, CellColumns),


    Fun1 = fun(Col, Str) ->
                   case Col of
                       {'$end',_Len      } -> lists:append([Str, ?CRLF]);
                       {"type", Len      } -> lists:append([Str, lists:duplicate(Len + 1, "-"), "+"]);
                       {"node", Len      } -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"]);
                       {_Other, Len      } -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"])
                   end
           end,
    Header1 = lists:foldl(Fun1, [], CellColumns),

    Fun2 = fun(Col, Str) ->
                   {Name, _} = Col,
                   case Col of
                       {'$end',_Len      } -> lists:append([Str, ?CRLF]);
                       {_Other, Len      } -> lists:append([Str, string:centre(Name, Len, $ ), ?SEPARATOR])
                   end
           end,
    Header2 = lists:foldl(Fun2, [], CellColumns),

    Fun3 = fun(N, List) ->
                   {Type, Alias, Version} = N,
                   Ret = lists:append([string:centre(Type,    lists:nth(1,LenPerCol)), ?SEPARATOR,
                                       string:left(Alias,     lists:nth(2,LenPerCol)), ?SEPARATOR,
                                       string:left(Version,   lists:nth(3,LenPerCol)), ?SEPARATOR,
                                       ?CRLF]),
                   List ++ [Ret]
           end,
    lists:foldl(Fun3, [" [Version of Node(s)]\r\n",
                       Header1,Header2, Header1], Nodes) ++ Header1 ++ ?CRLF.

%% @doc Format the result of a cluster node state
-spec(node_stat(string(), [tuple()]) ->
             string()).
node_stat(?SERVER_TYPE_GATEWAY, State) ->
    Version = leo_misc:get_value('version', State, []),
    Directories = leo_misc:get_value('dirs', State, []),
    HttpConf = leo_misc:get_value('http_conf', State, []),
    RingHashes = leo_misc:get_value('ring_checksum', State, []),
    Statistics = leo_misc:get_value('statistics', State, []),
    LogLevel = leo_misc:get_value('log_level', State, []),

    MaxChunkedObjs = leo_misc:get_value('max_chunked_objs', HttpConf, 0),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',  HttpConf, 0),
    MaxObjLen = MaxChunkedObjs * ChunkedObjLen,
    WatchdogProps = leo_misc:get_value('watchdog', State, []),

    io_lib:format(lists:append([
                                "-------------------------------+------------------\r\n",
                                "             Item              |       Value      \r\n",
                                "-------------------------------+------------------\r\n",
                                " Config-1: basic\r\n",
                                "--------------------------------------------------\r\n",
                                " [basic]\r\n",
                                "-------------------------------+------------------\r\n",
                                "                       version | ~s\r\n",
                                "                using protocol | ~w\r\n",
                                "                 log directory | ~s\r\n",
                                "                     log level | ~s\r\n",
                                "-------------------------------+------------------\r\n",
                                " [http server related for rest/s3 api]\r\n",
                                "-------------------------------+------------------\r\n",
                                "                listening port | ~w\r\n",
                                "            listening ssl port | ~w\r\n",
                                "           number of acceptors | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                " [cache-related]\r\n",
                                "-------------------------------+------------------\r\n",
                                "       http cache [true/false] | ~w\r\n",
                                "       number of cache_workers | ~w\r\n",
                                "                  cache expire | ~w\r\n",
                                "         cache max content len | ~w\r\n",
                                "            ram cache capacity | ~w\r\n",
                                "        disk cache capacity    | ~w\r\n",
                                "        disk cache threshold   | ~w\r\n",
                                "        disk cache data dir    | ~s\r\n",
                                "        disk cache journal dir | ~s\r\n",
                                "-------------------------------+------------------\r\n",
                                " [large object related]\r\n",
                                "-------------------------------+------------------\r\n",
                                "      max number of chunk objs | ~w\r\n",
                                "           chunk object length | ~w\r\n",
                                "             max object length | ~w\r\n",
                                "     reading  chunk obj length | ~w\r\n",
                                "     threshold of chunk length | ~w\r\n",
                                "                               | \r\n",
                                "      [PUT] worker pool size   | ~w\r\n",
                                "      [PUT] worker buffer size | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                " Config-2: watchdog\r\n",
                                "-------------------------------+------------------\r\n",
                                " [rex(rpc-proc)]               |\r\n",
                                "             watch interval(s) | ~w\r\n",
                                "        threshold mem capacity | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                " [cpu]                         |\r\n",
                                "              eanbled/disabled | ~w\r\n",
                                "             check interval(s) | ~w\r\n",
                                "        threshold cpu load avg | ~w\r\n",
                                "         threshold cpu util(%) | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                " [io]                          |\r\n",
                                "              enabled/disabled | ~w\r\n",
                                "             check interval(s) | ~w\r\n",
                                "        threshold input size/s | ~w\r\n",
                                "       threshold output size/s | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                " Status-1: RING hash\r\n",
                                "-------------------------------+------------------\r\n",
                                "             current ring hash | ~s\r\n",
                                "            previous ring hash | ~s\r\n",
                                "-------------------------------+------------------\r\n",
                                " Status-2: Erlang VM\r\n",
                                "-------------------------------+------------------\r\n",
                                "                    vm version | ~s\r\n",
                                "               total mem usage | ~w\r\n",
                                "              system mem usage | ~w\r\n",
                                "               procs mem usage | ~w\r\n",
                                "                 ets mem usage | ~w\r\n",
                                "                         procs | ~w/~w\r\n",
                                "                   kernel poll | ~w\r\n",
                                "              thread pool size | ~w\r\n",
                                "-------------------------------+------------------\r\n",
                                "\r\n"
                               ]),
                  [
                   %% config-1 [2]
                   Version,
                   leo_misc:get_value('handler', HttpConf, ''),
                   leo_misc:get_value('log', Directories, []),
                   LogLevel,
                   %% config-2 [17]
                   leo_misc:get_value('port', HttpConf, 0),
                   leo_misc:get_value('ssl_port', HttpConf, 0),
                   leo_misc:get_value('num_of_acceptors', HttpConf, 0),
                   leo_misc:get_value('http_cache', HttpConf, ''),
                   leo_misc:get_value('cache_workers', HttpConf, 0),
                   leo_misc:get_value('cache_expire', HttpConf, 0),
                   leo_misc:get_value('cache_max_content_len', HttpConf, 0),
                   leo_misc:get_value('cache_ram_capacity', HttpConf, 0),
                   leo_misc:get_value('cache_disc_capacity', HttpConf, 0),
                   leo_misc:get_value('cache_disc_threshold_len', HttpConf, 0),
                   leo_misc:get_value('cache_disc_dir_data', HttpConf, ""),
                   leo_misc:get_value('cache_disc_dir_journal', HttpConf, ""),

                   %% large-object
                   MaxChunkedObjs,
                   ChunkedObjLen,
                   MaxObjLen,
                   leo_misc:get_value('reading_chunked_obj_len', HttpConf, 0),
                   leo_misc:get_value('threshold_of_chunk_len', HttpConf, 0),
                   leo_misc:get_value('loh_put_worker_pool_size', HttpConf, 0),
                   leo_misc:get_value('loh_put_worker_buffer_size', HttpConf, 0),

                   %% config-3:watchdog
                   leo_misc:get_value('rex_interval', WatchdogProps),
                   leo_misc:get_value('rex_threshold_mem_capacity', WatchdogProps),
                   leo_manager_formatter_commons:exchange_value(
                     ?BOOL_TO_ENABLE,
                     leo_misc:get_value('cpu_enabled', WatchdogProps)),
                   leo_misc:get_value('cpu_interval', WatchdogProps),
                   leo_misc:get_value('cpu_threshold_cpu_load_avg', WatchdogProps),
                   leo_misc:get_value('cpu_threshold_cpu_util', WatchdogProps),
                   leo_manager_formatter_commons:exchange_value(
                     ?BOOL_TO_ENABLE,
                     leo_misc:get_value('io_enabled', WatchdogProps)),
                   leo_misc:get_value('io_interval', WatchdogProps),
                   leo_misc:get_value('io_threshold_input_per_sec', WatchdogProps),
                   leo_misc:get_value('io_threshold_output_per_sec', WatchdogProps),
                   %% status-1 [2]
                   leo_hex:integer_to_hex(leo_misc:get_value('ring_cur', RingHashes, 0), 8),
                   leo_hex:integer_to_hex(leo_misc:get_value('ring_prev', RingHashes, 0), 8),
                   %% status-2 [8]
                   leo_misc:get_value('vm_version', Statistics, []),
                   leo_misc:get_value('total_mem_usage', Statistics, 0),
                   leo_misc:get_value('system_mem_usage', Statistics, 0),
                   leo_misc:get_value('proc_mem_usage', Statistics, 0),
                   leo_misc:get_value('ets_mem_usage', Statistics, 0),
                   leo_misc:get_value('num_of_procs', Statistics, 0),
                   leo_misc:get_value('process_limit', Statistics, 0),
                   leo_misc:get_value('kernel_poll', Statistics, false),
                   leo_misc:get_value('thread_pool_size', Statistics, 0)
                  ]);

node_stat(?SERVER_TYPE_STORAGE, State) ->
    Version = leo_misc:get_value('version', State, []),
    NumOfVNodes = leo_misc:get_value('num_of_vnodes', State, []),
    _GrpLevel2 = leo_misc:get_value('grp_level_2', State, []),
    Directories = leo_misc:get_value('dirs', State, []),
    RingHashes = leo_misc:get_value('ring_checksum', State, []),
    Statistics = leo_misc:get_value('statistics', State, []),
    ObjContainer = leo_misc:get_value('avs', State, []),
    CustomItems = leo_misc:get_value('storage', Statistics, []),
    WatchdogProps = leo_misc:get_value('watchdog', State, []),
    LogLevel = leo_misc:get_value('log_level', State, []),

    MQConf_1 = leo_misc:get_value('mq_num_of_procs', State, []),
    MQConf_2 = leo_misc:get_value('mq_num_of_batch_process_max', State, []),
    MQConf_3 = leo_misc:get_value('mq_num_of_batch_process_reg', State, []),
    MQConf_4 = leo_misc:get_value('mq_interval_between_batch_procs_max', State, []),
    MQConf_5 = leo_misc:get_value('mq_interval_between_batch_procs_reg', State, []),

    AutoCompactionEnabled = leo_misc:get_value('auto_compaction_enabled', State),
    AutoCompactionConf_1 = leo_misc:get_value('auto_compaction_warn_active_size_ratio', State),
    AutoCompactionConf_2 = leo_misc:get_value('auto_compaction_threshold_active_size_ratio', State),
    AutoCompactionConf_3 = leo_misc:get_value('auto_compaction_parallel_procs', State),
    AutoCompactionConf_4 = leo_misc:get_value('auto_compaction_interval', State),

    CompactionConf_1 = leo_misc:get_value('limit_num_of_compaction_procs', State),
    CompactionConf_2 = leo_misc:get_value('compaction_num_of_batch_procs_max', State),
    CompactionConf_3 = leo_misc:get_value('compaction_num_of_batch_procs_reg', State),
    CompactionConf_4 = leo_misc:get_value('compaction_interval_between_batch_procs_max', State),
    CompactionConf_5 = leo_misc:get_value('compaction_interval_between_batch_procs_reg', State),

    ObjContainer_1 = lists:flatten(
                       lists:foldl(
                         fun(Items, SoFar) ->
                                 Path = leo_misc:get_value('path', Items, []),
                                 NumOfContainers = integer_to_list(
                                                     leo_misc:get_value('num_of_containers', Items, 0)),
                                 case SoFar of
                                     [] ->
                                         lists:append(["- path:[", Path, "]",
                                                       ", # of containers:", NumOfContainers]);
                                     _  ->
                                         lists:append([SoFar, "\r\n",
                                                       "                                                  | ",
                                                       "- path:[", Path, "]",
                                                       ", # of containers:", NumOfContainers])
                                 end
                         end, [], ObjContainer)),

    io_lib:format(lists:append([
                                "--------------------------------------+--------------------------------------\r\n",
                                "                Item                  |                 Value                \r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Config-1: basic\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "                              version | ~s\r\n",
                                "                     number of vnodes | ~w\r\n",
                                "                    object containers | ~s\r\n",
                                "                        log directory | ~s\r\n",
                                "                            log level | ~s\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Config-2: watchdog\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " [rex(rpc-proc)]                      |\r\n",
                                "                    check interval(s) | ~w\r\n",
                                "               threshold mem capacity | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " [cpu]                                |\r\n",
                                "                     enabled/disabled | ~w\r\n",
                                "                    check interval(s) | ~w\r\n",
                                "               threshold cpu load avg | ~w\r\n",
                                "                threshold cpu util(%) | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " [disk]                               |\r\n",
                                "                     enabled/disalbed | ~w\r\n",
                                "                    check interval(s) | ~w\r\n",
                                "                threshold disk use(%) | ~w\r\n",
                                "               threshold disk util(%) | ~w\r\n",
                                "                    threshold rkb(kb) | ~w\r\n",
                                "                    threshold wkb(kb) | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Config-3: message-queue\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "                   number of procs/mq | ~w\r\n",
                                "        number of batch-procs of msgs | max:~w, regular:~w\r\n",
                                "   interval between batch-procs (ms)  | max:~w, regular:~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Config-4: autonomic operation\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " [auto-compaction]                    |\r\n",
                                "                     enabled/disabled | ~w\r\n",
                                "        warning active size ratio (%) | ~w\r\n",
                                "      threshold active size ratio (%) | ~w\r\n",
                                "             number of parallel procs | ~w\r\n",
                                "                        exec interval | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Config-5: data-compaction\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "  limit of number of compaction procs | ~w\r\n",
                                "        number of batch-procs of objs | max:~w, regular:~w\r\n",
                                "   interval between batch-procs (ms)  | max:~w, regular:~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Status-1: RING hash\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "                    current ring hash | ~s\r\n",
                                "                   previous ring hash | ~s\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Status-2: Erlang VM\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "                           vm version | ~s\r\n",
                                "                      total mem usage | ~w\r\n",
                                "                     system mem usage | ~w\r\n",
                                "                      procs mem usage | ~w\r\n",
                                "                        ets mem usage | ~w\r\n",
                                "                                procs | ~w/~w\r\n",
                                "                          kernel_poll | ~w\r\n",
                                "                     thread_pool_size | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                " Status-3: Number of messages in MQ\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "                 replication messages | ~w\r\n",
                                "                  vnode-sync messages | ~w\r\n",
                                "                   rebalance messages | ~w\r\n",
                                "--------------------------------------+--------------------------------------\r\n",
                                "\r\n"]),
                  [
                   %% Basic Conf
                   Version,
                   NumOfVNodes,
                   ObjContainer_1,
                   leo_misc:get_value('log', Directories, []),
                   LogLevel,
                   %% Watchdog
                   leo_misc:get_value('rex_interval', WatchdogProps),
                   leo_misc:get_value('rex_threshold_mem_capacity', WatchdogProps),
                   leo_manager_formatter_commons:exchange_value(
                     ?BOOL_TO_ENABLE,
                     leo_misc:get_value('cpu_enabled', WatchdogProps)),
                   leo_misc:get_value('cpu_interval', WatchdogProps),
                   leo_misc:get_value('cpu_threshold_cpu_load_avg', WatchdogProps),
                   leo_misc:get_value('cpu_threshold_cpu_util', WatchdogProps),
                   leo_manager_formatter_commons:exchange_value(
                     ?BOOL_TO_ENABLE,
                     leo_misc:get_value('disk_enabled', WatchdogProps)),
                   leo_misc:get_value('disk_interval', WatchdogProps),
                   leo_misc:get_value('disk_threshold_use', WatchdogProps),
                   leo_misc:get_value('disk_threshold_util', WatchdogProps),
                   leo_misc:get_value('disk_threshold_rkb', WatchdogProps),
                   leo_misc:get_value('disk_threshold_wkb', WatchdogProps),
                   %% MQ
                   MQConf_1,
                   MQConf_2,
                   MQConf_3,
                   MQConf_4,
                   MQConf_5,
                   %% Auto-compaction
                   leo_manager_formatter_commons:exchange_value(
                     ?BOOL_TO_ENABLE, AutoCompactionEnabled),
                   AutoCompactionConf_1,
                   AutoCompactionConf_2,
                   AutoCompactionConf_3,
                   AutoCompactionConf_4,
                   %% Compaction
                   CompactionConf_1,
                   CompactionConf_2,
                   CompactionConf_3,
                   CompactionConf_4,
                   CompactionConf_5,
                   %% RING
                   leo_hex:integer_to_hex(leo_misc:get_value('ring_cur', RingHashes, 0), 8),
                   leo_hex:integer_to_hex(leo_misc:get_value('ring_prev', RingHashes, 0), 8),
                   %% Erlang-VM
                   leo_misc:get_value('vm_version', Statistics, []),
                   leo_misc:get_value('total_mem_usage', Statistics, 0),
                   leo_misc:get_value('system_mem_usage', Statistics, 0),
                   leo_misc:get_value('proc_mem_usage', Statistics, 0),
                   leo_misc:get_value('ets_mem_usage', Statistics, 0),
                   leo_misc:get_value('num_of_procs', Statistics, 0),
                   leo_misc:get_value('process_limit', Statistics, 0),
                   leo_misc:get_value('kernel_poll', Statistics, false),
                   leo_misc:get_value('thread_pool_size', Statistics, 0),
                   %% MQ
                   leo_misc:get_value('num_of_replication_msg', CustomItems, 0),
                   leo_misc:get_value('num_of_sync_vnode_msg', CustomItems, 0),
                   leo_misc:get_value('num_of_rebalance_msg', CustomItems, 0)
                  ]).


%% @doc Status of compaction
-spec(compact_status(#compaction_stats{}) ->
             string()).
compact_status(#compaction_stats{status = Status,
                                 total_num_of_targets = TotalNumOfTargets,
                                 num_of_pending_targets = Targets1,
                                 num_of_ongoing_targets = Targets2,
                                 num_of_reserved_targets = Targets3,
                                 latest_exec_datetime = LatestExecDate}) ->
    Date = case LatestExecDate of
               0 -> ?NULL_DATETIME;
               _ -> leo_date:date_format(LatestExecDate)
           end,

    io_lib:format(lists:append(["        current status: ~w\r\n",
                                " last compaction start: ~s\r\n",
                                "         total targets: ~w\r\n",
                                "  # of pending targets: ~w\r\n",
                                "  # of ongoing targets: ~w\r\n",
                                "  # of out of targets : ~w\r\n",
                                "\r\n"]),
                  [Status, Date, TotalNumOfTargets, Targets1, Targets2, Targets3]).


%% @doc Format the result of a list of storge-status
-spec(du(summary | detail, {integer(), integer(), integer(),
                            integer(), integer(), integer()} | list()) ->
             string()).
du(summary, {TotalNum, ActiveNum, TotalSize, ActiveSize, LastStart, LastEnd}) ->
    Fun = fun(0) -> ?NULL_DATETIME;
             (D) -> leo_date:date_format(D)
          end,
    Ratio = ?ratio_of_active_size(ActiveSize, TotalSize),
    io_lib:format(lists:append([" active number of objects: ~w\r\n",
                                "  total number of objects: ~w\r\n",
                                "   active size of objects: ~w\r\n",
                                "    total size of objects: ~w\r\n",
                                "     ratio of active size: ~w%\r\n",
                                "    last compaction start: ~s\r\n",
                                "      last compaction end: ~s\r\n\r\n"]),
                  [ActiveNum, TotalNum,
                   ActiveSize, TotalSize, Ratio,
                   Fun(LastStart), Fun(LastEnd)]);

du(detail, StatsList) when is_list(StatsList) ->
    Fun = fun(#storage_stats{file_path = FilePath,
                             compaction_hist = Histories,
                             total_sizes = TotalSize,
                             active_sizes = ActiveSize,
                             total_num = Total,
                             active_num = Active}, Acc) ->
                  {Start_1, End_1, Duration_1, CompactionRet_1} =
                      case length(Histories) of
                          0 ->
                              {?NULL_DATETIME, ?NULL_DATETIME, 0, ''};
                          _ ->
                              #compaction_hist{
                                 start_datetime = Start,
                                 end_datetime = End,
                                 duration = Duration,
                                 result = CompactionRet} = hd(Histories),
                              {leo_date:date_format(Start),
                               leo_date:date_format(End),
                               Duration,
                               CompactionRet}
                      end,
                  Ratio = ?ratio_of_active_size(ActiveSize, TotalSize),

                  lists:append([Acc,
                                io_lib:format(
                                  lists:append(["                file path: ~s\r\n",
                                                " active number of objects: ~w\r\n",
                                                "  total number of objects: ~w\r\n",
                                                "   active size of objects: ~w\r\n"
                                                "    total size of objects: ~w\r\n",
                                                "     ratio of active size: ~w%\r\n",
                                                "    last compaction start: ~s\r\n"
                                                "      last compaction end: ~s\r\n"
                                                "                 duration: ~ws\r\n"
                                                "                   result: ~w\r\n\r\n"
                                               ]),
                                  [FilePath,
                                   Active,
                                   Total,
                                   ActiveSize,
                                   TotalSize,
                                   Ratio,
                                   Start_1,
                                   End_1,
                                   Duration_1,
                                   CompactionRet_1
                                  ])]);
             (_Error, Acc) ->
                  Acc
          end,
    lists:append([lists:foldl(Fun, "[du(storage stats)]\r\n", StatsList), "\r\n"]);
du(_, _) ->
    [].


%% @doc Format the result of mq-stats
-spec(mq_stats(Stats) ->
             string() when Stats::[tuple()]).
mq_stats([]) ->
    [];
mq_stats(Stats) ->
    Header = "              id                |    state    | number of msgs | batch of msgs  |    interval    |                 description                 \r\n"
        ++   "--------------------------------+-------------+----------------|----------------|----------------|---------------------------------------------\r\n",
    Output = lists:foldl(
               fun(#mq_state{id = Id,
                             state = ConsumerStats,
                             desc = Desc}, Acc) ->
                       State       = leo_misc:get_value(?MQ_CNS_PROP_STATUS, ConsumerStats),
                       NumOfMsgs   = leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS, ConsumerStats, 0),
                       BatchOfMsgs = leo_misc:get_value(?MQ_CNS_PROP_BATCH_OF_MSGS, ConsumerStats, 0),
                       Interval    = leo_misc:get_value(?MQ_CNS_PROP_INTERVAL, ConsumerStats, 0),
                       lists:append([Acc,
                                     string:left(" " ++ atom_to_list(Id), 31), ?SEPARATOR,
                                     string:centre(atom_to_list(State), 11), ?SEPARATOR,
                                     string:left(integer_to_list(NumOfMsgs), 14), ?SEPARATOR,
                                     string:left(integer_to_list(BatchOfMsgs), 14), ?SEPARATOR,
                                     string:left(integer_to_list(Interval), 14), ?SEPARATOR,
                                     string:left(Desc, 44), ?CRLF])
               end, Header, Stats),
    Output.


%% @doc Format the result of s3-gen-key
%%
-spec(credential(string(), string()) ->
             string()).
credential(AccessKeyId, SecretAccessKey) ->
    io_lib:format("  access-key-id: ~s\r\n  secret-access-key: ~s\r\n\r\n",
                  [AccessKeyId, SecretAccessKey]).


%% @doc Format the result of s3-users
%%
-spec(users([#?S3_USER{}]) ->
             string()).
users(Owners) ->
    Col_1_Len = lists:foldl(fun(User, Acc) ->
                                    UserId = leo_misc:get_value(user_id, User),
                                    Len = length(binary_to_list(UserId)),
                                    case (Len > Acc) of
                                        true ->
                                            Len;
                                        false ->
                                            Acc
                                    end
                            end, 7, Owners),
    Col_2_Len = 7,
    Col_3_Len = 22,
    Col_4_Len = 26,

    Header = lists:append([string:left("user_id", Col_1_Len), " | ",
                           string:left("role_id", Col_2_Len), " | ",
                           string:left("access_key_id", Col_3_Len), " | ",
                           string:left("created_at", Col_4_Len), "\r\n",
                           lists:duplicate(Col_1_Len, "-"), "-+-",
                           lists:duplicate(Col_2_Len, "-"), "-+-",
                           lists:duplicate(Col_3_Len, "-"), "-+-",
                           lists:duplicate(Col_4_Len, "-"), "\r\n"]),
    Fun = fun(User, Acc) ->
                  UserId = leo_misc:get_value(user_id, User),
                  RoleId = leo_misc:get_value(role_id, User),
                  AccessKeyId = leo_misc:get_value(access_key_id, User),
                  CreatedAt = leo_misc:get_value(created_at, User),

                  UserIdStr = binary_to_list(UserId),
                  RoleIdStr = integer_to_list(RoleId),
                  AccessKeyIdStr = binary_to_list(AccessKeyId),
                  Acc ++ io_lib:format("~s | ~s | ~s | ~s\r\n",
                                       [string:left(UserIdStr, Col_1_Len),
                                        string:left(RoleIdStr, Col_2_Len),
                                        string:left(AccessKeyIdStr, Col_3_Len),
                                        leo_date:date_format(CreatedAt)])
          end,
    lists:append([lists:foldl(Fun, Header, Owners), "\r\n"]).


%% @doc Format the result of a list of endpoints
-spec(endpoints(list(tuple())) ->
             string()).
endpoints(EndPoints) ->
    Col_1_Len = lists:foldl(fun({_, EP, _}, Acc) ->
                                    Len = byte_size(EP),
                                    case (Len > Acc) of
                                        true ->
                                            Len;
                                        false ->
                                            Acc
                                    end
                            end, 8, EndPoints),
    Col_2_Len = 26,

    Header = lists:append([string:left("endpoint", Col_1_Len),
                           " | ",
                           string:left("created at", Col_2_Len),
                           "\r\n",
                           lists:duplicate(Col_1_Len, "-"),
                           "-+-", lists:duplicate(Col_2_Len, "-"),
                           "\r\n"]),
    Fun = fun({endpoint, EP, Created}, Acc) ->
                  EndpointStr = binary_to_list(EP),
                  Acc ++ io_lib:format("~s | ~s\r\n",
                                       [string:left(EndpointStr,Col_1_Len),
                                        leo_date:date_format(Created)])
          end,
    lists:append([lists:foldl(Fun, Header, EndPoints), "\r\n"]).


%% @doc Format the result of a list of buckets
-spec(buckets([#?BUCKET{}]) ->
             string()).
buckets(Buckets) ->
    Col_1_MinLen = 12, %% cluster-id
    Col_2_MinLen = 8,  %% bucket-name
    Col_3_MinLen = 6,  %% owner
    Col_4_MinLen = 12, %% permissions
    {Col_1_Len, Col_2_Len,
     Col_3_Len, Col_4_Len} =
        lists:foldl(fun(#bucket_dto{name = Bucket,
                                    owner = #user_credential{user_id= Owner},
                                    acls = Permissions,
                                    cluster_id = ClusterId,
                                    created_at = _CreatedAt},
                        {C1, C2, C3, C4}) ->
                            ClusterIdStr = atom_to_list(ClusterId),
                            BucketStr = binary_to_list(Bucket),
                            OwnerStr = binary_to_list(Owner),
                            PermissionsStr = leo_s3_bucket:aclinfo_to_str(Permissions),
                            Len1 = length(ClusterIdStr),
                            Len2 = length(BucketStr),
                            Len3 = length(OwnerStr),
                            Len4 = length(PermissionsStr),

                            {case (Len1 > C1) of
                                 true -> Len1;
                                 false -> C1
                             end,
                             case (Len2 > C2) of
                                 true -> Len2;
                                 false -> C2
                             end,
                             case (Len3 > C3) of
                                 true -> Len3;
                                 false -> C3
                             end,
                             case (Len4 > C4) of
                                 true -> Len4;
                                 false -> C4
                             end}
                    end, {Col_1_MinLen, Col_2_MinLen,
                          Col_3_MinLen, Col_4_MinLen}, Buckets),
    Col_5_Len = 28, %% erasure-coding related items
    Col_6_Len = 26, %% created at

    Header = lists:append(
               [
                string:left("cluster id",  Col_1_Len), " | ",
                string:left("bucket", Col_2_Len), " | ",
                string:left("owner", Col_3_Len), " | ",
                string:left("permissions", Col_4_Len), " | ",
                string:left("redundancy method", Col_5_Len), " | ",
                string:left("created at", Col_6_Len), "\r\n",

                lists:duplicate(Col_1_Len, "-"), "-+-",
                lists:duplicate(Col_2_Len, "-"), "-+-",
                lists:duplicate(Col_3_Len, "-"), "-+-",
                lists:duplicate(Col_4_Len, "-"), "-+-",
                lists:duplicate(Col_5_Len, "-"), "-+-",
                lists:duplicate(Col_6_Len, "-"), "\r\n"
               ]),

    Fun = fun(#bucket_dto{name = Bucket,
                          owner = #user_credential{user_id= Owner},
                          acls = Permissions,
                          cluster_id = ClusterId,
                          redundancy_method = RedMethod,
                          cp_params = CPParams,
                          ec_params = ECParams,
                          created_at = Created_1}, Acc) ->
                  ClusterIdStr = atom_to_list(ClusterId),
                  BucketStr = binary_to_list(Bucket),
                  OwnerStr = binary_to_list(Owner),
                  PermissionsStr = leo_s3_bucket:aclinfo_to_str(Permissions),

                  {ok, RedOptions} = leo_redundant_manager_api:get_options(),
                  ECParamsStr = get_redundancy_method_str(
                                  RedMethod, CPParams, ECParams, RedOptions),
                  Created_2  = case (Created_1 > 0) of
                                   true ->
                                       leo_date:date_format(Created_1);
                                   false ->
                                       []
                               end,
                  Acc ++ io_lib:format("~s | ~s | ~s | ~s | ~s | ~s\r\n",
                                       [string:left(ClusterIdStr, Col_1_Len),
                                        string:left(BucketStr, Col_2_Len),
                                        string:left(OwnerStr, Col_3_Len),
                                        string:left(PermissionsStr, Col_4_Len),
                                        string:left(ECParamsStr, Col_5_Len),
                                        Created_2])
          end,
    lists:append([lists:foldl(Fun, Header, Buckets), "\r\n"]).


%% @doc Retrieve redundancy method str of the bucket
%% @private
get_redundancy_method_str(RedMethod, CPParams, ECParams, RedOptions) ->
    case RedMethod of
        'erasure_code' ->
            {ECParam_K, ECParam_M} = ECParams,
            lists:append([atom_to_list(RedMethod),
                          ",",
                          " {k:", integer_to_list(ECParam_K),
                          ", m:", integer_to_list(ECParam_M),
                          "}"
                         ]);
        _ ->
            {N, W, R, D} =
                case CPParams of
                    {_,_,_,_} ->
                        CPParams;
                    _ ->
                        {leo_misc:get_value('n', RedOptions),
                         leo_misc:get_value('w', RedOptions),
                         leo_misc:get_value('r', RedOptions),
                         leo_misc:get_value('d', RedOptions)}
                end,
            lists:append([atom_to_list(RedMethod),
                          ",",
                          " {n:", integer_to_list(N),
                          ", w:", integer_to_list(W),
                          ", r:", integer_to_list(R),
                          ", d:", integer_to_list(D),
                          "}"
                         ])
    end.


%% @doc Format the result of a list of buckets
-spec(bucket_by_access_key(list(#?BUCKET{})) ->
             string()).
bucket_by_access_key(Buckets) ->
    Col_1_MinLen = 8,
    Col_2_MinLen = 12,
    {Col_1_Len, Col_2_Len} =
        lists:foldl(fun(#?BUCKET{name = Bucket,
                                 acls = Permissions}, {C1, C2}) ->
                            BucketStr = binary_to_list(Bucket),
                            PermissionsStr = leo_s3_bucket:aclinfo_to_str(Permissions),
                            Len1 = length(BucketStr),
                            Len2 = length(PermissionsStr),
                            {case (Len1 > C1) of
                                 true -> Len1;
                                 false -> C1
                             end,
                             case (Len2 > C2) of
                                 true -> Len2;
                                 false -> C2
                             end
                            }
                    end, {Col_1_MinLen, Col_2_MinLen}, Buckets),
    Col_3_Len = 28,
    Col_4_Len = 26,
    Header = lists:append(
               [string:left("bucket", Col_1_Len), " | ",
                string:left("permissions", Col_2_Len), " | ",
                string:left("redundancy method", Col_3_Len), " | ",
                string:left("created at", Col_4_Len), "\r\n",

                lists:duplicate(Col_1_Len, "-"), "-+-",
                lists:duplicate(Col_2_Len, "-"), "-+-",
                lists:duplicate(Col_3_Len, "-"), "-+-",
                lists:duplicate(Col_4_Len, "-"), "\r\n"]),

    Fun = fun(#?BUCKET{name = Bucket1,
                       acls = Permissions1,
                       redundancy_method = RedMethod,
                       cp_params = CPParams,
                       ec_params = ECParams,
                       created_at = Created1}, Acc) ->
                  BucketStr = binary_to_list(Bucket1),
                  PermissionsStr = leo_s3_bucket:aclinfo_to_str(Permissions1),

                  {ok, RedOptions} = leo_redundant_manager_api:get_options(),
                  ECParamsStr = get_redundancy_method_str(
                                  RedMethod, CPParams, ECParams, RedOptions),
                  Created2  = case (Created1 > 0) of
                                  true  -> leo_date:date_format(Created1);
                                  false -> []
                              end,
                  Acc ++ io_lib:format("~s | ~s | ~s | ~s\r\n",
                                       [string:left(BucketStr, Col_1_Len),
                                        string:left(PermissionsStr, Col_2_Len),
                                        string:left(ECParamsStr, Col_3_Len),
                                        Created2])
          end,
    lists:append([lists:foldl(Fun, Header, Buckets), "\r\n"]).


%% @doc Format the states of a deletion-bucket
-spec(del_bucket_stats(Stats) ->
             string() when Stats::[#del_bucket_state{}]).
del_bucket_stats(Stats) ->
    Col_1_MinLen = 28,
    Col_1_Len =
        lists:foldl(fun(#del_bucket_state{node = Node}, C) ->
                            Len = length(atom_to_list(Node)),
                            case (Len > C) of
                                true ->
                                    Len;
                                false ->
                                    C
                            end
                    end, Col_1_MinLen, Stats),
    Col_2_Len = 16,
    Col_3_Len = 28,

    Header = lists:append(
               [string:left("node", Col_1_Len), " | ",
                string:left("state", Col_2_Len), " | ",
                string:left("timestamp", Col_3_Len), "\r\n",

                lists:duplicate(Col_1_Len, "-"), "-+-",
                lists:duplicate(Col_2_Len, "-"), "-+-",
                lists:duplicate(Col_3_Len, "-"), "\r\n"]),

    Fun = fun(#del_bucket_state{node = Node,
                                state = State,
                                timestamp = TS}, Acc) ->
                  NodeStr = atom_to_list(Node),
                  StateStr = ?del_bucket_state_str(State),
                  TS_1  = case (TS > 1) of
                              true ->
                                  leo_date:date_format(TS);
                              false ->
                                  []
                          end,
                  Acc ++ io_lib:format("~s | ~s | ~s\r\n",
                                       [string:left(NodeStr, Col_1_Len),
                                        string:left(StateStr, Col_2_Len),
                                        string:left(TS_1, Col_3_Len)
                                       ])
          end,
    lists:append([lists:foldl(Fun, Header, lists:sort(Stats)), "\r\n"]).


%% @doc Format the states of a deletion-bucket
-spec(del_bucket_stats_all(Stats) ->
             string() when BucketName::binary(),
                           Stats::[ {BucketName, [#del_bucket_state{}]} ]).
del_bucket_stats_all(Stats) ->
    lists:foldl(fun({BucketName, NodeStateList}, Acc) ->
                        Acc_1 = lists:append([Acc, "- Bucket: ",  binary_to_list(BucketName), "\r\n"]),
                        Acc_2 = lists:append([Acc_1, del_bucket_stats(NodeStateList), "\r\n"]),
                        Acc_2
                end, [], Stats).


%% @doc Format the result of a list of acls
-spec(acls(acls()) ->
             string()).
acls(ACLs) ->
    Col_1_Len = lists:foldl(fun(#bucket_acl_info{user_id = User} = _ACL, C1) ->
                                    UserStr = binary_to_list(User),
                                    Len = length(UserStr),
                                    case (Len > C1) of
                                        true -> Len;
                                        false -> C1
                                    end
                            end, 14, ACLs),
    Col_2_Len = 24, % @todo to be calcurated
    Header = lists:append(
               [string:left("access_key_id", Col_1_Len), " | ",
                string:left("permissions", Col_2_Len), "\r\n",
                lists:duplicate(Col_1_Len, "-"), "-+-",
                lists:duplicate(Col_2_Len, "-"), "\r\n"]),

    Fun = fun(#bucket_acl_info{user_id = User, permissions = Permissions} = _ACL, Acc) ->
                  UserStr = binary_to_list(User),
                  FormatStr = case length(Permissions) of
                                  0 ->
                                      "~s | private\r\n";
                                  1 ->
                                      "~s | ~s\r\n";
                                  N ->
                                      lists:flatten(lists:append(["~s | ~s",
                                                                  lists:duplicate(N-1, ", ~s"),
                                                                  "\r\n"]))
                              end,
                  Acc ++ io_lib:format(FormatStr,
                                       [string:left(UserStr, Col_1_Len)] ++ Permissions)
          end,
    lists:append([lists:foldl(Fun, Header, ACLs), "\r\n"]).


%% @doc Cluster statuses
-spec(cluster_status(list()) ->
             string()).
cluster_status(Stats) ->
    Col1Min = 10, %% cluster-id
    Col_1_Len = lists:foldl(fun(N, Acc) ->

                                    Len = length(atom_to_list(leo_misc:get_value('cluster_id', N))),
                                    case (Len > Acc) of
                                        true -> Len;
                                        false -> Acc
                                    end
                            end, Col1Min, Stats),
    Col2Min = 10, %% dc-id
    Col_2_Len = lists:foldl(fun(N, Acc) ->
                                    Len = length(atom_to_list(leo_misc:get_value('dc_id', N))),
                                    case (Len > Acc) of
                                        true -> Len;
                                        false -> Acc
                                    end
                            end, Col2Min, Stats),
    Col_3_Len = 12, %% status
    Col_4_Len = 14, %% # of storages
    Col_5_Len = 28, %% updated-at

    Header = lists:append(
               [string:centre("cluster id", Col_1_Len), " | ",
                string:centre("dc id", Col_2_Len), " | ",
                string:centre("status", Col_3_Len), " | ",
                string:centre("# of storages", Col_4_Len), " | ",
                string:centre("updated at", Col_5_Len), "\r\n",

                lists:duplicate(Col_1_Len, "-"), "-+-",
                lists:duplicate(Col_2_Len, "-"), "-+-",
                lists:duplicate(Col_3_Len, "-"), "-+-",
                lists:duplicate(Col_4_Len, "-"), "-+-",
                lists:duplicate(Col_5_Len, "-"), "\r\n"]),

    Fun = fun(Items, Acc) ->
                  ClusterId_1 = leo_misc:get_value('cluster_id', Items),
                  ClusterId_2 = case is_atom(ClusterId_1) of
                                    true -> atom_to_list(ClusterId_1);
                                    false -> ClusterId_1
                                end,
                  DCId_1 = leo_misc:get_value('dc_id', Items),
                  DCId_2 = case is_atom(DCId_1) of
                               true -> atom_to_list(DCId_1);
                               false -> DCId_1
                           end,
                  Status = atom_to_list(leo_misc:get_value('status', Items)),
                  Storages = integer_to_list(leo_misc:get_value('members', Items)),
                  UpdatedAt = leo_misc:get_value('updated_at', Items),
                  UpdatedAt_1 = case (UpdatedAt > 0) of
                                    true -> leo_date:date_format(UpdatedAt);
                                    false -> []
                                end,
                  Acc ++ io_lib:format("~s | ~s | ~s | ~s | ~s\r\n",
                                       [string:left(ClusterId_2, Col_1_Len),
                                        string:left(DCId_2, Col_2_Len),
                                        string:left(Status, Col_3_Len),
                                        string:right(Storages, Col_4_Len),
                                        UpdatedAt_1])
          end,
    lists:append([lists:foldl(Fun, Header, Stats), "\r\n"]).


%% @doc Format the result of an assigned file
-spec(whereis(list()) ->
             string()).
whereis(AssignedInfo) ->
    Col_2_Len = lists:foldl(fun(N, Acc) ->
                                    Len = length(element(1,N)),
                                    case (Len > Acc) of
                                        true -> Len;
                                        false -> Acc
                                    end
                            end, 0, AssignedInfo) + 5,
    CellColumns = [{"del?", 6},
                   {"node", Col_2_Len},
                   {"ring address", 36},
                   {"size", 10},
                   {"checksum", 12},
                   %% === NOTE: for 1.4 >>>
                   %% {"redundancy method", 36},
                   %% <<<
                   {"has children", 14},
                   {"total chunks", 14},
                   {"clock", 14},
                   {"when", 28},
                   {'$end', 0}],
    LenPerCol = lists:map(fun({_, Len})-> Len end, CellColumns),

    Fun1 = fun(Col, Str) ->
                   case Col of
                       {'$end',_Len} -> lists:append([Str, ?CRLF]);
                       {"del?", Len} -> lists:append([Str, lists:duplicate(Len + 1, "-"), "+"]);
                       {"node", Len} -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"]);
                       {"when", Len} -> lists:append([Str, lists:duplicate(Len + 0, "-")]);
                       {_Other, Len} -> lists:append([Str, lists:duplicate(Len + 2, "-"), "+"])
                   end
           end,
    Header_1 = lists:foldl(Fun1, [], CellColumns),

    Fun2 = fun(Col, Str) ->
                   {Name, _} = Col,
                   case Col of
                       {'$end', _  } -> lists:append([Str, ?CRLF]);
                       {"when", Len} -> lists:append([Str, string:centre(Name, Len, $ )]);
                       {_Other, Len} -> lists:append([Str, string:centre(Name, Len, $ ), ?SEPARATOR])
                   end
           end,
    Header_2 = lists:foldl(Fun2, [], CellColumns),

    Fun3 = fun(N, List) ->
                   Ret = case N of
                             {Node, not_found} ->
                                 lists:append([string:left([], lists:nth(1,LenPerCol)), ?SEPARATOR,
                                               string:left(Node, lists:nth(2,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(3,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(4,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(5,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(6,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(7,LenPerCol)), ?SEPARATOR,
                                               string:left([], lists:nth(8,LenPerCol)), ?SEPARATOR,
                                               %% === NOTE: for 1.4 >>>
                                               %% string:left([], lists:nth(9,LenPerCol)), ?SEPARATOR,
                                               %% <<<
                                               ?CRLF]);
                             {Node, ItemL} ->
                                 VNodeId = leo_misc:get_value('addr_id', ItemL),
                                 DSize = leo_misc:get_value('dsize', ItemL),
                                 ChunkedObjs = leo_misc:get_value('cnumber', ItemL),
                                 Clock = leo_misc:get_value('clock', ItemL),
                                 Timestamp = leo_misc:get_value('timestamp', ItemL),
                                 Checksum = leo_misc:get_value('checksum', ItemL),

                                 %% === NOTE: for 1.4 >>>
                                 %% RedMethod = leo_misc:get_value('redundancy_method', ItemL),
                                 %% ECLib = leo_misc:get_value('ec_lib', ItemL),
                                 %% ECParams = leo_misc:get_value('ec_params', ItemL),
                                 %% <<<

                                 CSize = leo_misc:get_value('csize', ItemL),
                                 HasChildren = leo_misc:get_value('has_children', ItemL, false),
                                 DelFlag = leo_misc:get_value('del', ItemL),

                                 %% === NOTE: for 1.4 >>>
                                 %% RedMethodStr = atom_to_list(RedMethod),
                                 %% RedMethod_1 = case RedMethod of
                                 %%                   'erasure_code' ->
                                 %%                       {ECParam_K, ECParam_M} = ECParams,
                                 %%                       lists:append([RedMethodStr, ", ",
                                 %%                                     "{k:", integer_to_list(ECParam_K), ","
                                 %%                                     " m:", integer_to_list(ECParam_M), ","
                                 %%                                     " ", atom_to_list(ECLib),
                                 %%                                     "}"]);
                                 %%                   'copy' ->
                                 %%                       RedMethodStr
                                 %%               end,
                                 %% <<<

                                 HasChildren_1 = (HasChildren andalso CSize > 0),
                                 FormattedDate = leo_date:date_format(Timestamp),
                                 DelStr = case DelFlag of
                                              0 -> ?SPACE;
                                              _ -> "*"
                                          end,
                                 lists:append([string:centre(DelStr, lists:nth(1, LenPerCol)), ?SEPARATOR,
                                               string:left(Node, lists:nth(2, LenPerCol)), ?SEPARATOR,
                                               string:left(leo_hex:integer_to_hex(VNodeId, 8), lists:nth(3, LenPerCol)), ?SEPARATOR,
                                               string:right(leo_file:dsize(DSize), lists:nth(4, LenPerCol)), ?SEPARATOR,
                                               string:right(string:sub_string(leo_hex:integer_to_hex(Checksum, 8), 1, 10),
                                                            lists:nth(5,LenPerCol)), ?SEPARATOR,
                                               %% === NOTE: for 1.4 >>>
                                               %% string:left(RedMethod_1, lists:nth(6,LenPerCol)), ?SEPARATOR,
                                               %% string:left(atom_to_list(HasChildren_1), lists:nth(7,LenPerCol)), ?SEPARATOR,
                                               %% string:right(integer_to_list(ChunkedObjs), lists:nth(8, LenPerCol)), ?SEPARATOR,
                                               %% string:left(leo_hex:integer_to_hex(Clock, 8), lists:nth(9, LenPerCol)), ?SEPARATOR,
                                               %% <<<
                                               %% === NOTE: for 1.3 >>>
                                               string:left(atom_to_list(HasChildren_1), lists:nth(6,LenPerCol)), ?SEPARATOR,
                                               string:right(integer_to_list(ChunkedObjs), lists:nth(7, LenPerCol)), ?SEPARATOR,
                                               string:left(leo_hex:integer_to_hex(Clock, 8), lists:nth(8, LenPerCol)), ?SEPARATOR,
                                               %% <<<
                                               FormattedDate,
                                               ?CRLF])
                         end,
                   List ++ [Ret]
           end,
    lists:foldl(Fun3, [Header_1, Header_2, Header_1], AssignedInfo) ++ ?CRLF.


%% @doc Format the result of a NFS mount key
nfs_mnt_key(Key) ->
    io_lib:format("~s\r\n", [Key]).


%% @doc Format the result of a list of a histories
-spec(histories(list(#history{})) ->
             string()).
histories(Histories) ->
    Fun = fun(#history{id      = Id,
                       command = Command,
                       created = Created}, Acc) ->
                  Acc ++ io_lib:format("~s | ~s | ~s\r\n",
                                       [string:left(integer_to_list(Id), 4),
                                        leo_date:date_format(Created),
                                        Command])
          end,
    lists:foldl(Fun, "[Histories]\r\n", Histories).


authorized() ->
    io_lib:format("OK\r\n\r\n", []).

user_id() ->
    io_lib:format("~s\r\n",["user-id:"]).

password() ->
    io_lib:format("~s\r\n",["password:"]).
