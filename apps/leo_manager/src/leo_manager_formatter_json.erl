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
-module(leo_manager_formatter_json).

-include("leo_manager.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_user.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([ok/0, error/1, error/2, help/0, version/1, login/2,
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

-define(output_ok(), gen_json({[{result, <<"OK">>}]})).
-define(output_error_1(Cause), gen_json({[{error, leo_misc:any_to_binary(Cause)}]})).


%% @doc Format 'ok'
-spec(ok() ->
             binary()).
ok() ->
    gen_json({[{result, <<"OK">>}]}).


%% @doc Format 'error'
-spec(error(atom() | string()) ->
             binary()).
error(not_found)  ->
    gen_json({[{error,<<"not found">>}]});
error(nodedown)  ->
    gen_json({[{error,<<"node down">>}]});
error(Cause) ->
    gen_json({[{error, leo_misc:any_to_binary(Cause)}]}).

-spec(error(atom()|string()|binary(), any()) ->
             binary()).
error(Node, Cause) when is_atom(Node) ->
    gen_json({[{error,
                {[{<<"node">>,  leo_misc:any_to_binary(Node)},
                  {<<"cause">>, leo_misc:any_to_binary(Cause)}]}
               }]});
error(Node, Cause) ->
    gen_json({[{error,
                {[{<<"node">>,  leo_misc:any_to_binary(Node)},
                  {<<"cause">>, leo_misc:any_to_binary(Cause)}]}
               }]}).


%% @doc Format 'help'
-spec(help() ->
             binary()).
help() ->
    <<>>.


%% @doc Format 'version'
-spec(version(string()|binary()) ->
             binary()).
version(Version) ->
    gen_json({[{result, leo_misc:any_to_binary(Version)}]}).


%% @doc Format 'login'
-spec(login(#?S3_USER{}, [tuple()]) ->
             binary()).
login(User, Credential) ->
    gen_json({[{<<"user">>,
                {[{<<"id">>,            User#?S3_USER.id},
                  {<<"role_id">>,       User#?S3_USER.role_id},
                  {<<"access_key_id">>, leo_misc:get_value('access_key_id',     Credential)},
                  {<<"secret_key">>,    leo_misc:get_value('secret_access_key', Credential)},
                  {<<"created_at">>,    leo_misc:any_to_binary(leo_date:date_format(User#?S3_USER.created_at))}
                 ]}}
              ]}).


%% @doc Format 'bad nodes'
-spec(bad_nodes([atom()]) ->
             binary()).
bad_nodes(BadNodes) ->
    Cause = lists:flatten(
              lists:foldl(fun(Node, [] ) ->        io_lib:format("~w",  [Node]);
                             (Node, Acc) -> Acc ++ io_lib:format(",~w", [Node])
                          end, [], BadNodes)),
    gen_json({[{error, leo_misc:any_to_binary(Cause)}]}).


%% @doc Format the result of the system-info and a list of the cluster-nodes
-spec(system_info_and_nodes_stat([tuple()]) ->
             binary()).
system_info_and_nodes_stat(Props) ->
    SystemConf = leo_misc:get_value('system_config', Props),
    Version    = leo_misc:get_value('version',       Props),
    [RH0, RH1] = leo_misc:get_value('ring_hash',     Props),
    Nodes      = leo_misc:get_value('nodes',         Props),

    NodeInfo = case Nodes of
                   [] -> [];
                   _  ->
                       lists:map(
                         fun({Type, NodeName, NodeState, RingHash0, RingHash1, When}) ->
                                 NewRingHash0 = case is_integer(RingHash0) of
                                                    true  -> integer_to_list(RingHash0);
                                                    false -> RingHash0
                                                end,
                                 NewRingHash1 = case is_integer(RingHash1) of
                                                    true  -> integer_to_list(RingHash1);
                                                    false -> RingHash1
                                                end,
                                 {[{<<"type">>,      leo_misc:any_to_binary(Type)},
                                   {<<"node">>,      leo_misc:any_to_binary(NodeName)},
                                   {<<"state">>,     leo_misc:any_to_binary(NodeState)},
                                   {<<"ring_cur">>,  leo_misc:any_to_binary(NewRingHash0)},
                                   {<<"ring_prev">>, leo_misc:any_to_binary(NewRingHash1)},
                                   {<<"when">>,      leo_misc:any_to_binary(leo_date:date_format(When))}
                                  ]}
                         end, Nodes)
               end,

    ClusterId_1 = SystemConf#?SYSTEM_CONF.cluster_id,
    ClusterId_2 = atom_to_list(ClusterId_1),
    DCId_1 = SystemConf#?SYSTEM_CONF.dc_id,
    DCId_2 = atom_to_list(DCId_1),

    gen_json({[{<<"system_info">>,
                {[{<<"version">>,    leo_misc:any_to_binary(Version)},
                  {<<"cluster_id">>, leo_misc:any_to_binary(ClusterId_2)},
                  {<<"dc_id">>,      leo_misc:any_to_binary(DCId_2)},
                  {<<"n">>, SystemConf#?SYSTEM_CONF.n},
                  {<<"r">>, SystemConf#?SYSTEM_CONF.r},
                  {<<"w">>, SystemConf#?SYSTEM_CONF.w},
                  {<<"d">>, SystemConf#?SYSTEM_CONF.d},
                  %% @Deprecated: `dc_awareness_replicas`
                  {<<"dc_awareness_replicas">>, SystemConf#?SYSTEM_CONF.num_of_dc_replicas},
                  {<<"mdcr_n">>, SystemConf#?SYSTEM_CONF.num_of_dc_replicas},
                  {<<"mdcr_r">>, SystemConf#?SYSTEM_CONF.mdcr_r},
                  {<<"mdcr_w">>, SystemConf#?SYSTEM_CONF.mdcr_w},
                  {<<"mdcr_d">>, SystemConf#?SYSTEM_CONF.mdcr_d},
                  {<<"rack_awareness_replicas">>, SystemConf#?SYSTEM_CONF.num_of_rack_replicas},
                  {<<"ring_size">>,      SystemConf#?SYSTEM_CONF.bit_of_ring},
                  {<<"ring_hash_cur">>,  RH0},
                  {<<"ring_hash_prev">>, RH1},
                  {<<"max_mdc_targets">>,SystemConf#?SYSTEM_CONF.max_mdc_targets}
                 ]}},
               {<<"node_list">>, NodeInfo}
              ]}).


%% @doc Format the result of a list of versions
-spec(version_all([tuple()]) ->
             binary()).
version_all(Nodes) ->
    NodeInfo = case Nodes of
                   [] -> [];
                   _  ->
                       lists:map(
                         fun({Type, NodeName, Version}) ->
                                 {[{<<"type">>,      leo_misc:any_to_binary(Type)},
                                   {<<"node">>,      leo_misc:any_to_binary(NodeName)},
                                   {<<"version">>,   leo_misc:any_to_binary(Version)}
                                  ]}
                         end, Nodes)
               end,
    gen_json({[{<<"result">>, NodeInfo}]}).


%% @doc Format the result of a cluster node state
-spec(node_stat(string(), [tuple()]) ->
             binary()).
node_stat(?SERVER_TYPE_GATEWAY, State) ->
    Version      = leo_misc:get_value('version',       State, []),
    Directories  = leo_misc:get_value('dirs',          State, []),
    HttpConf     = leo_misc:get_value('http_conf',     State, []),
    RingHashes   = leo_misc:get_value('ring_checksum', State, []),
    Statistics   = leo_misc:get_value('statistics',    State, []),

    MaxChunkedObjs = leo_misc:get_value('max_chunked_objs', HttpConf, 0),
    ChunkedObjLen  = leo_misc:get_value('chunked_obj_len',  HttpConf, 0),
    MaxObjLen = MaxChunkedObjs * ChunkedObjLen,
    WatchdogProps = leo_misc:get_value('watchdog', State, []),

    gen_json({[{<<"node_stat">>,
                {[
                  %% config-1
                  {<<"version">>, leo_misc:any_to_binary(Version)},
                  {<<"log_dir">>, leo_misc:any_to_binary(leo_misc:get_value('log', Directories, []))},

                  %% config-2
                  {<<"ring_cur">>,  leo_misc:any_to_binary(
                                      leo_hex:integer_to_hex(
                                        leo_misc:get_value('ring_cur', RingHashes, 0), 8))},
                  {<<"ring_prev">>, leo_misc:any_to_binary(
                                      leo_hex:integer_to_hex(
                                        leo_misc:get_value('ring_prev', RingHashes, 0), 8))},
                  {<<"vm_version">>,       leo_misc:any_to_binary(leo_misc:get_value('vm_version', Statistics, []))},
                  {<<"total_mem_usage">>,  leo_misc:get_value('total_mem_usage',  Statistics, 0)},
                  {<<"system_mem_usage">>, leo_misc:get_value('system_mem_usage', Statistics, 0)},
                  {<<"procs_mem_usage">>,  leo_misc:get_value('proc_mem_usage',   Statistics, 0)},
                  {<<"ets_mem_usage">>,    leo_misc:get_value('ets_mem_usage',    Statistics, 0)},
                  {<<"num_of_procs">>,     leo_misc:get_value('num_of_procs',     Statistics, 0)},
                  {<<"limit_of_procs">>,   leo_misc:get_value('process_limit',    Statistics, 0)},
                  {<<"kernel_poll">>,      leo_misc:any_to_binary(leo_misc:get_value('kernel_poll', Statistics, false))},
                  {<<"thread_pool_size">>, leo_misc:get_value('thread_pool_size', Statistics, 0)},

                  %% config-2
                  {<<"handler">>,                  leo_misc:any_to_binary(leo_misc:get_value('handler', HttpConf, ''))},
                  {<<"port">>,                     leo_misc:get_value('port',             HttpConf, 0)},
                  {<<"ssl_port">>,                 leo_misc:get_value('ssl_port',         HttpConf, 0)},
                  {<<"num_of_acceptors">>,         leo_misc:get_value('num_of_acceptors', HttpConf, 0)},
                  {<<"http_cache">>,               leo_misc:any_to_binary(leo_misc:get_value('http_cache', HttpConf, ''))},
                  {<<"cache_workers">>,            leo_misc:get_value('cache_workers',            HttpConf, 0)},
                  {<<"cache_expire">>,             leo_misc:get_value('cache_expire',             HttpConf, 0)},
                  {<<"cache_ram_capacity">>,       leo_misc:get_value('cache_ram_capacity',       HttpConf, 0)},
                  {<<"cache_disc_capacity">>,      leo_misc:get_value('cache_disc_capacity',      HttpConf, 0)},
                  {<<"cache_disc_threshold_len">>, leo_misc:get_value('cache_disc_threshold_len', HttpConf, 0)},
                  {<<"cache_disc_dir_data">>,      leo_misc:any_to_binary(
                                                     leo_misc:get_value('cache_disc_dir_data', HttpConf, ""))},
                  {<<"cache_disc_dir_journal">>,   leo_misc:any_to_binary(
                                                     leo_misc:get_value('cache_disc_dir_journal', HttpConf, ""))},
                  {<<"cache_max_content_len">>,    leo_misc:get_value('cache_max_content_len', HttpConf, 0)},

                  %% large-object
                  {<<"max_chunked_objs">>,           MaxChunkedObjs},
                  {<<"chunked_obj_len">>,            ChunkedObjLen},
                  {<<"max_len_for_obj">>,            MaxObjLen},
                  {<<"reading_chunked_obj_len">>,    leo_misc:get_value('reading_chunked_obj_len', HttpConf, 0)},
                  {<<"threshold_of_chunk_len">>,     leo_misc:get_value('threshold_of_chunk_len',  HttpConf, 0)},
                  {<<"loh_put_worker_pool_size">>,   leo_misc:get_value('loh_put_worker_pool_size',  HttpConf, 0)},
                  {<<"loh_put_worker_buffer_size">>, leo_misc:get_value('loh_put_worker_buffer_size',  HttpConf, 0)},

                  %% watchdog
                  {<<"wd_rex_interval">>,                leo_misc:get_value('rex_interval',                WatchdogProps)},
                  {<<"wd_rex_threshold_mem_capacity">>,  leo_misc:get_value('rex_threshold_mem_capacity',  WatchdogProps)},
                  {<<"wd_cpu_enabled">>,                 list_to_binary(
                                                           atom_to_list(
                                                             leo_manager_formatter_commons:exchange_value(
                                                               ?BOOL_TO_ENABLE,
                                                               leo_misc:get_value('cpu_enabled',  WatchdogProps))))},
                  {<<"wd_cpu_interval">>,                leo_misc:get_value('cpu_interval',                WatchdogProps)},
                  {<<"wd_cpu_threshold_cpu_load_avg">>,  leo_misc:get_value('cpu_threshold_cpu_load_avg',  WatchdogProps)},
                  {<<"wd_cpu_threshold_cpu_util">>,      leo_misc:get_value('cpu_threshold_cpu_util',      WatchdogProps)},
                  {<<"wd_io_enabled">>,                  list_to_binary(
                                                           atom_to_list(
                                                             leo_manager_formatter_commons:exchange_value(
                                                               ?BOOL_TO_ENABLE,
                                                               leo_misc:get_value('io_enabled',   WatchdogProps))))},
                  {<<"wd_io_interval">>,                 leo_misc:get_value('io_interval',                 WatchdogProps)},
                  {<<"wd_io_threshold_input_per_sec">>,  leo_misc:get_value('io_threshold_input_per_sec',  WatchdogProps)},
                  {<<"wd_io_threshold_output_per_sec">>, leo_misc:get_value('io_threshold_output_per_sec', WatchdogProps)}
                 ]}}
              ]});

node_stat(?SERVER_TYPE_STORAGE, State) ->
    Version     = leo_misc:get_value('version',       State, []),
    NumOfVNodes = leo_misc:get_value('num_of_vnodes', State, -1),
    GrpLevel2   = leo_misc:get_value('grp_level_2',   State, []),
    Directories = leo_misc:get_value('dirs',          State, []),
    RingHashes  = leo_misc:get_value('ring_checksum', State, []),
    Statistics  = leo_misc:get_value('statistics',    State, []),
    MsgQueue    = leo_misc:get_value('storage', Statistics, []),
    WatchdogProps = leo_misc:get_value('watchdog', State, []),

    gen_json({[{<<"node_stat">>,
                {[{<<"version">>,          leo_misc:any_to_binary(Version)},
                  {<<"num_of_vnodes">>,    NumOfVNodes},
                  {<<"grp_level_2">>,      leo_misc:any_to_binary(GrpLevel2)},
                  {<<"log_dir">>,          leo_misc:any_to_binary(leo_misc:get_value('log', Directories, []))},
                  {<<"ring_cur">>,         leo_misc:any_to_binary(leo_hex:integer_to_hex(leo_misc:get_value('ring_cur',  RingHashes, 0), 8))},
                  {<<"ring_prev">>,        leo_misc:any_to_binary(leo_hex:integer_to_hex(leo_misc:get_value('ring_prev', RingHashes, 0), 8))},
                  {<<"vm_version">>,       leo_misc:any_to_binary(leo_misc:get_value('vm_version', Statistics, []))},
                  {<<"total_mem_usage">>,  leo_misc:get_value('total_mem_usage',  Statistics, 0)},
                  {<<"system_mem_usage">>, leo_misc:get_value('system_mem_usage', Statistics, 0)},
                  {<<"procs_mem_usage">>,  leo_misc:get_value('proc_mem_usage',   Statistics, 0)},
                  {<<"ets_mem_usage">>,    leo_misc:get_value('ets_mem_usage',    Statistics, 0)},
                  {<<"num_of_procs">>,     leo_misc:get_value('num_of_procs',     Statistics, 0)},
                  {<<"limit_of_procs">>,   leo_misc:get_value('process_limit',    Statistics, 0)},
                  {<<"kernel_poll">>,      leo_misc:any_to_binary(leo_misc:get_value('kernel_poll', Statistics, false))},
                  {<<"thread_pool_size">>, leo_misc:get_value('thread_pool_size', Statistics, 0)},
                  {<<"replication_msgs">>, leo_misc:get_value('num_of_replication_msg', MsgQueue, 0)},
                  {<<"sync_vnode_msgs">>,  leo_misc:get_value('num_of_sync_vnode_msg',  MsgQueue, 0)},
                  {<<"rebalance_msgs">>,   leo_misc:get_value('num_of_rebalance_msg',   MsgQueue, 0)},
                  %% watchdog
                  {<<"wd_rex_interval">>,                leo_misc:get_value('rex_interval',                WatchdogProps)},
                  {<<"wd_rex_threshold_mem_capacity">>,  leo_misc:get_value('rex_threshold_mem_capacity',  WatchdogProps)},
                  {<<"wd_cpu_enabled">>,                 list_to_binary(
                                                           atom_to_list(
                                                             leo_manager_formatter_commons:exchange_value(
                                                               ?BOOL_TO_ENABLE,
                                                               leo_misc:get_value('cpu_enabled', WatchdogProps))))},
                  {<<"wd_cpu_interval">>,                leo_misc:get_value('cpu_interval',                WatchdogProps)},
                  {<<"wd_cpu_threshold_cpu_load_avg">>,  leo_misc:get_value('cpu_threshold_cpu_load_avg',  WatchdogProps)},
                  {<<"wd_cpu_threshold_cpu_util">>,      leo_misc:get_value('cpu_threshold_cpu_util',      WatchdogProps)},
                  {<<"wd_disk_enabled">>,  list_to_binary(
                                             atom_to_list(
                                               leo_manager_formatter_commons:exchange_value(
                                                 ?BOOL_TO_ENABLE,
                                                 leo_misc:get_value('disk_enabled', WatchdogProps))))},
                  {<<"wd_disk_interval">>,            leo_misc:get_value('disk_interval',            WatchdogProps)},
                  {<<"wd_disk_threshold_disk_use">>,  leo_misc:get_value('disk_threshold_disk_use',  WatchdogProps)},
                  {<<"wd_disk_threshold_disk_util">>, leo_misc:get_value('disk_threshold_disk_util', WatchdogProps)},
                  %% mq
                  {<<"mq_num_of_procs">>,              leo_misc:get_value('mq_num_of_procs', State)},
                  {<<"mq_num_of_batch_process_max">>,  leo_misc:get_value('mq_num_of_batch_process_max',  State)},
                  {<<"mq_num_of_batch_process_reg">>,  leo_misc:get_value('mq_num_of_batch_process_reg',  State)},
                  {<<"mq_interval_between_batch_procs_max">>,  leo_misc:get_value('mq_interval_between_batch_procs_max',  State)},
                  {<<"mq_interval_between_batch_procs_reg">>,  leo_misc:get_value('mq_interval_between_batch_procs_reg',  State)},
                  %% compaction
                  {<<"auto_compaction_enabled">>, list_to_binary(
                                                    atom_to_list(
                                                      leo_manager_formatter_commons:exchange_value(
                                                        ?BOOL_TO_ENABLE,
                                                        leo_misc:get_value('auto_compaction_enabled', State))))},
                  {<<"auto_compaction_warn_active_size_ratio">>,       leo_misc:get_value('auto_compaction_warn_active_size_ratio', State)},
                  {<<"auto_compaction_threshold_active_size_ratio">>,  leo_misc:get_value('auto_compaction_threshold_active_size_ratio', State)},
                  {<<"auto_compaction_parallel_procs">>,               leo_misc:get_value('auto_compaction_parallel_procs', State)},
                  {<<"auto_compaction_interval">>,                     leo_misc:get_value('auto_compaction_interval',       State)},
                  {<<"limit_num_of_compaction_procs">>,                leo_misc:get_value('limit_num_of_compaction_procs',  State)},
                  {<<"compaction_num_of_batch_procs_max">>,            leo_misc:get_value('compaction_num_of_batch_procs_max',  State)},
                  {<<"compaction_num_of_batch_procs_reg">>,            leo_misc:get_value('compaction_num_of_batch_procs_reg',  State)},
                  {<<"compaction_interval_between_batch_procs_max">>,  leo_misc:get_value('compaction_interval_between_batch_procs_max',  State)},
                  {<<"compaction_interval_between_batch_procs_reg">>,  leo_misc:get_value('compaction_interval_between_batch_procs_reg',  State)}
                 ]}}
              ]}).


%% @doc Status of compaction
-spec(compact_status(#compaction_stats{}) ->
             binary()).
compact_status(#compaction_stats{status = Status,
                                 total_num_of_targets    = TotalNumOfTargets,
                                 num_of_pending_targets  = Targets1,
                                 num_of_ongoing_targets  = Targets2,
                                 num_of_reserved_targets = Targets3,
                                 latest_exec_datetime    = LatestExecDate}) ->
    Date = case LatestExecDate of
               0 -> ?NULL_DATETIME;
               _ -> leo_date:date_format(LatestExecDate)
           end,

    gen_json({[{<<"compaction_status">>,
                {[{<<"status">>,                 Status},
                  {<<"last_compaction_start">>,  leo_misc:any_to_binary(Date)},
                  {<<"total_targets">>,          TotalNumOfTargets},
                  {<<"num_of_pending_targets">>, Targets1},
                  {<<"num_of_ongoing_targets">>, Targets2},
                  {<<"num_of_out_of_targets">>,  Targets3}
                 ]}}
              ]}).


%% @doc Format the result of `du`
-spec(du(summary | detail, {integer(), integer(), integer(), integer(), integer(), integer()} | list()) ->
             binary()).
du(summary, {TotalNum, ActiveNum, TotalSize, ActiveSize, LastStart, LastEnd}) ->
    StartStr = case LastStart of
                   0 -> ?NULL_DATETIME;
                   _ -> leo_date:date_format(LastStart)
               end,
    EndStr = case LastEnd of
                 0 -> ?NULL_DATETIME;
                 _ -> leo_date:date_format(LastEnd)
             end,
    Ratio = ?ratio_of_active_size(ActiveSize, TotalSize),

    gen_json({[
               {<<"active_num_of_objects">>,  ActiveNum},
               {<<"total_num_of_objects">>,   TotalNum},
               {<<"active_size_of_objects">>, ActiveSize},
               {<<"total_size_of_objects">>,  TotalSize},
               {<<"ratio_of_active_size">>,   Ratio},
               {<<"last_compaction_start">>,  leo_misc:any_to_binary(StartStr)},
               {<<"last_compaction_end">>,    leo_misc:any_to_binary(EndStr)}
              ]});

du(detail, StatsList) when is_list(StatsList) ->
    JSON = lists:map(fun(#storage_stats{file_path = FilePath,
                                        compaction_hist = Histories,
                                        total_sizes  = TotalSize,
                                        active_sizes = ActiveSize,
                                        total_num    = Total,
                                        active_num   = Active}) ->
                             {LatestStart_1, LatestEnd_1, Duration_1, CompactionRet_1} =
                                 case length(Histories) of
                                     0 ->
                                         {?NULL_DATETIME, ?NULL_DATETIME, 0, ''};
                                     _ ->
                                         #compaction_hist{
                                            start_datetime = Start,
                                            end_datetime   = End,
                                            duration = Duration,
                                            result = CompactionRet} = hd(Histories),
                                         {leo_date:date_format(Start),
                                          leo_date:date_format(End),
                                          Duration,
                                          CompactionRet}
                                 end,
                             Ratio = ?ratio_of_active_size(ActiveSize, TotalSize),

                             {[{<<"file_path">>,              leo_misc:any_to_binary(FilePath)},
                               {<<"active_num_of_objects">>,  Active},
                               {<<"total_num_of_objects">>,   Total},
                               {<<"active_size_of_objects">>, ActiveSize},
                               {<<"total_size_of_objects">>,  TotalSize},
                               {<<"ratio_of_active_size">>,   Ratio},
                               {<<"last_compaction_start">>,  leo_misc:any_to_binary(LatestStart_1)},
                               {<<"last_compaction_end">>,    leo_misc:any_to_binary(LatestEnd_1)},
                               {<<"compaction_duration">>,    Duration_1},
                               {<<"compaction_result">>,      leo_misc:any_to_binary(CompactionRet_1)}
                              ]};
                        (_) ->
                             []
                     end, StatsList),
    gen_json(JSON);
du(_, _) ->
    gen_json([]).


%% @doc Format result of mq-stats
-spec(mq_stats(Stats) ->
             binary() when Stats::[tuple()]).
mq_stats([]) ->
    gen_json([]);
mq_stats(Stats) ->
    JSON = lists:map(
             fun(#mq_state{id = Id,
                           state = ConsumerStats,
                           desc = Desc}) ->
                     State       = leo_misc:get_value(?MQ_CNS_PROP_STATUS,        ConsumerStats),
                     NumOfMsgs   = leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS,   ConsumerStats, 0),
                     BatchOfMsgs = leo_misc:get_value(?MQ_CNS_PROP_BATCH_OF_MSGS, ConsumerStats, 0),
                     Interval    = leo_misc:get_value(?MQ_CNS_PROP_INTERVAL,      ConsumerStats, 0),
                     {[{<<"id">>,             leo_misc:any_to_binary(atom_to_list(Id))},
                       {<<"state">>,          leo_misc:any_to_binary(atom_to_list(State))},
                       {<<"number_of_msgs">>, NumOfMsgs},
                       {<<"batch_of_msgs">>,  BatchOfMsgs},
                       {<<"interval">>,       Interval},
                       {<<"description">>,    leo_misc:any_to_binary(Desc)}
                      ]}
             end, Stats),
    gen_json({[{<<"mq_stats">>, JSON}]}).


%% @doc Format the resulst of s3-gen-key
-spec(credential(binary(), binary()) ->
             binary()).
credential(AccessKeyId, SecretAccessKey) ->
    gen_json({[
               {access_key_id,     AccessKeyId},
               {secret_access_key, SecretAccessKey}
              ]}).


%% @doc Format the result of s3-owers
-spec(users(list(#user_credential{})) ->
             binary()).
users(Owners) ->
    JSON = lists:map(fun(User) ->
                             UserId      = leo_misc:get_value(user_id,       User),
                             RoleId      = leo_misc:get_value(role_id,       User),
                             AccessKeyId = leo_misc:get_value(access_key_id, User),
                             CreatedAt   = leo_misc:get_value(created_at,    User),
                             {[{<<"access_key_id">>, AccessKeyId},
                               {<<"user_id">>,       leo_misc:any_to_binary(UserId)},
                               {<<"role_id">>,       RoleId},
                               {<<"created_at">>,    leo_misc:any_to_binary(leo_date:date_format(CreatedAt))}
                              ]}
                     end, Owners),
    gen_json({[{<<"users">>, JSON}]}).


%% @doc Format the result of a list of endpoints
-spec(endpoints([tuple()]) ->
             binary()).
endpoints(EndPoints) ->
    JSON = lists:map(fun({endpoint, EP, CreatedAt}) ->
                             {[{<<"endpoint">>,   EP},
                               {<<"created_at">>, leo_misc:any_to_binary(leo_date:date_format(CreatedAt))}
                              ]}
                     end, EndPoints),
    gen_json({[{<<"endpoints">>, JSON}]}).


%% @doc Format the result of a list of buckets
-spec(buckets([#bucket_dto{}]) ->
             binary()).
buckets(Buckets) ->
    JSON = lists:map(fun(#bucket_dto{name  = Bucket,
                                     owner = #user_credential{user_id = Owner},
                                     acls  = Permissions,
                                     cluster_id = ClusterId,
                                     created_at = CreatedAt}) ->
                             CreatedAt_1  = case (CreatedAt > 0) of
                                                true  -> leo_date:date_format(CreatedAt);
                                                false -> []
                                            end,
                             PermissionsStr =
                                 string:join([atom_to_list(Item) ||
                                                 #bucket_acl_info{permissions = [Item|_]} <- Permissions],
                                             ","),
                             {[{<<"bucket">>,      Bucket},
                               {<<"owner">>,       leo_misc:any_to_binary(Owner)},
                               {<<"permissions">>, leo_misc:any_to_binary(PermissionsStr)},
                               {<<"cluster_id">>,  leo_misc:any_to_binary(ClusterId)},
                               {<<"created_at">>,  leo_misc:any_to_binary(CreatedAt_1)}
                              ]}
                     end, Buckets),
    gen_json({[{<<"buckets">>, JSON}]}).


%% @doc Format the result of bucket-by-access-key
-spec(bucket_by_access_key([#?BUCKET{}]) ->
             binary()).
bucket_by_access_key(Buckets) ->
    JSON = lists:map(fun(#?BUCKET{name = Bucket,
                                  acls = Permissions,
                                  created_at = CreatedAt}) ->
                             PermissionsStr = string:join([atom_to_list(Item)
                                                           || #bucket_acl_info{permissions = [Item|_]}
                                                                  <- Permissions], ","),
                             CreatedAt_1  = case (CreatedAt > 0) of
                                                true  -> leo_date:date_format(CreatedAt);
                                                false -> []
                                            end,
                             {[{<<"bucket">>,      Bucket},
                               {<<"permissions">>, leo_misc:any_to_binary(PermissionsStr)},
                               {<<"created_at">>,  leo_misc:any_to_binary(CreatedAt_1)}
                              ]}
                     end, Buckets),
    gen_json({[{<<"buckets">>, JSON}]}).


%% @doc Format the states of a deletion-bucket
-spec(del_bucket_stats(Stats) ->
             binary() when Stats::[#del_bucket_state{}]).
del_bucket_stats(Stats) ->
    JSON = lists:map(fun(#del_bucket_state{node = Node,
                                           state = State,
                                           timestamp = TS}) ->
                             TS_1  = case (TS > 1) of
                                         true ->
                                             leo_date:date_format(TS);
                                         false ->
                                             []
                                     end,


                             {[{<<"node">>, leo_misc:any_to_binary(Node)},
                               {<<"state">>, leo_misc:any_to_binary(State)},
                               {<<"state_str">>, leo_misc:any_to_binary(?del_bucket_state_str(State))},
                               {<<"timestamp">>,  leo_misc:any_to_binary(TS_1)}
                              ]}
                     end, Stats),
    gen_json({[{<<"de_bucket_state">>, JSON}]}).


%% @doc Format the states of a deletion-bucket
-spec(del_bucket_stats_all(Stats) ->
             binary() when BucketName::binary(),
                           Stats::[ {BucketName, [#del_bucket_state{}]} ]).
del_bucket_stats_all(Stats) ->
    JSON = lists:map(fun({BucketName, NodeStateList}) ->
                             SubJSON = lists:map(
                                         fun(#del_bucket_state{node = Node,
                                                               state = State,
                                                               timestamp = TS}) ->
                                                 TS_1  = case (TS > 1) of
                                                             true ->
                                                                 leo_date:date_format(TS);
                                                             false ->
                                                                 []
                                                         end,
                                                 {[{<<"node">>, leo_misc:any_to_binary(Node)},
                                                   {<<"state">>, leo_misc:any_to_binary(State)},
                                                   {<<"timestamp">>,  leo_misc:any_to_binary(TS_1)}
                                                  ]}
                                         end, NodeStateList),
                             {[{BucketName, SubJSON}]}
                     end, Stats),
    gen_json({[{<<"de_bucket_state_all">>, JSON}]}).



%% @doc Format the result of a list of acls
-spec(acls([#bucket_acl_info{}]) ->
             binary()).
acls(ACLs) ->
    JSON = lists:map(fun(#bucket_acl_info{user_id = UserId, permissions = Permissions}) ->
                             {[{<<"user_id">>,   UserId},
                               {<<"permissions">>, Permissions}
                              ]}
                     end, ACLs),
    gen_json({[{<<"acls">>, JSON}]}).


%% @doc Format the result of a cluster's status
-spec(cluster_status([tuple()]) ->
             binary()).
cluster_status(Stats) ->
    JSON = lists:map(fun(Items) ->
                             ClusterId_1 = leo_misc:get_value('cluster_id', Items),
                             ClusterId_2 = case is_atom(ClusterId_1) of
                                               true  -> atom_to_list(ClusterId_1);
                                               false -> ClusterId_1
                                           end,
                             DCId_1 = leo_misc:get_value('dc_id', Items),
                             DCId_2 = case is_atom(DCId_1) of
                                          true  -> atom_to_list(DCId_1);
                                          false -> DCId_1
                                      end,
                             Status = leo_misc:get_value('status', Items),
                             NumOfStorages = leo_misc:get_value('members', Items),
                             UpdatedAt = leo_misc:get_value('updated_at', Items),
                             UpdatedAt_1 = case (UpdatedAt > 0) of
                                               true  -> leo_date:date_format(UpdatedAt);
                                               false -> []
                                           end,
                             {[{<<"cluster_id">>, leo_misc:any_to_binary(ClusterId_2)},
                               {<<"dc_id">>,      leo_misc:any_to_binary(DCId_2)},
                               {<<"status">>,     leo_misc:any_to_binary(Status)},
                               {<<"num_of_storages">>, leo_misc:any_to_binary(NumOfStorages)},
                               {<<"updated_at">>,      leo_misc:any_to_binary(UpdatedAt_1)}
                              ]}
                     end, Stats),
    gen_json({[{<<"cluster_stats">>, JSON}]}).


%% @doc Format the result of an assigned file
-spec(whereis([tuple()]) ->
             binary()).
whereis(AssignedInfo) ->
    JSON = lists:map(fun({Node, not_found}) ->
                             {[{<<"node">>, leo_misc:any_to_binary(Node)},
                               {<<"vnode_id">>, <<>>},
                               {<<"size">>, <<>>},
                               {<<"num_of_chunks">>, 0},
                               {<<"clock">>,  <<>>},
                               {<<"checksum">>, <<>>},
                               {<<"timestamp">>, <<>>},
                               {<<"delete">>, 0}
                              ]};
                        ({Node, ItemL}) ->
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
                             HasChildren = leo_misc:get_value('has_children', ItemL),
                             DelFlag = leo_misc:get_value('del', ItemL),

                             %% === NOTE: for 1.4 >>>
                             %% {ECParam_K, ECParam_M} = case ECParams of
                             %%                              {_,_} ->
                             %%                                  ECParams;
                             %%                              _ ->
                             %%                                  {0,0}
                             %%                          end,
                             %% <<<

                             {[{<<"node">>, leo_misc:any_to_binary(Node)},
                               {<<"vnode_id">>, leo_misc:any_to_binary(leo_hex:integer_to_hex(VNodeId, 8))},
                               {<<"size">>, DSize},
                               {<<"num_of_chunks">>, ChunkedObjs},
                               {<<"clock">>, leo_misc:any_to_binary(leo_hex:integer_to_hex(Clock, 8))},
                               {<<"checksum">>, leo_misc:any_to_binary(leo_hex:integer_to_hex(Checksum, 8))},
                               {<<"timestamp">>, leo_misc:any_to_binary(leo_date:date_format(Timestamp))},
                               %% === NOTE: for 1.4 >>>
                               %% {<<"redundancy_method">>, list_to_binary(atom_to_list(RedMethod))},
                               %% {<<"erasure_code_lib">>, list_to_binary(atom_to_list(ECLib))},
                               %% {<<"erasure_param_k">>, ECParam_K},
                               %% {<<"erasure_param_m">>, ECParam_M},
                               %% <<<
                               {<<"chunk_size">>, CSize},
                               {<<"has_children">>, list_to_binary(atom_to_list(HasChildren))},
                               {<<"delete">>, DelFlag}
                              ]}
                     end, AssignedInfo),
    gen_json({[{<<"assigned_info">>, JSON}]}).


%% @doc Format the result of a NFS mount key
nfs_mnt_key(Key) ->
    gen_json({[{nfs_mnt_key, list_to_binary(Key)}]}).


%% @doc Format the result of a list of histories
-spec(histories(_) ->
             binary()).
histories(_) ->
    <<>>.


-spec(authorized() ->
             binary()).
authorized() ->
    <<>>.

-spec(user_id() ->
             binary()).
user_id() ->
    <<>>.


-spec(password() ->
             binary()).
password() ->
    <<>>.


%%----------------------------------------------------------------------
%% Inner function(s)
%%----------------------------------------------------------------------
%% @doc Generate a JSON-format doc
-spec(gen_json(tuple()|[tuple()]) ->
             binary()).
gen_json(JSON) ->
    case catch jiffy:encode(JSON) of
        {'EXIT', _} ->
            [];
        Result ->
            <<Result/binary, ?CRLF>>
    end.
