%%====================================================================
%%
%% LeoStorage
%%
%% Copyright (c) 2012-2018 Rakuten, Inc.
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
%% LeoFS Storage - Constant/Macro/Record
%%
%%====================================================================

%% @doc default-values.
-define(SHUTDOWN_WAITING_TIME, 2000).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(RETRY_TIMES, 5).

-ifdef(TEST).
-define(TIMEOUT, 1000).
-define(DEF_REQ_TIMEOUT, 1000).
-else.
-define(TIMEOUT, 5000).
-define(DEF_REQ_TIMEOUT, 30000).
-endif.

%% @doc operationg-methods.
-define(CMD_GET, get).
-define(CMD_PUT, put).
-define(CMD_DELETE, delete).
-define(CMD_HEAD, head).
-type(request_verb() :: ?CMD_GET |
                        ?CMD_PUT |
                        ?CMD_DELETE |
                        ?CMD_HEAD
                        ).

-type(etag_ret() :: {etag, non_neg_integer()}).


%% @doc queue-related.
-define(QUEUE_ID_PER_OBJECT, 'leo_per_object_queue').
-define(QUEUE_ID_SYNC_BY_VNODE_ID, 'leo_sync_by_vnode_id_queue').
-define(QUEUE_ID_DIRECTORY, 'leo_directory_queue').
-define(QUEUE_ID_REBALANCE, 'leo_rebalance_queue').
-define(QUEUE_ID_ASYNC_DELETION, 'leo_async_deletion_queue').
-define(QUEUE_ID_RECOVERY_NODE, 'leo_recovery_node_queue').
-define(QUEUE_ID_SYNC_OBJ_WITH_DC, 'leo_sync_obj_with_dc_queue').
-define(QUEUE_ID_COMP_META_WITH_DC, 'leo_comp_meta_with_dc_queue').
-define(QUEUE_ID_DEL_DIR, 'leo_delete_dir_queue').
-define(QUEUE_ID_REQ_DEL_DIR, 'leo_req_delete_dir_queue').
%% -define(QUEUE_ID_DEL_BUCKET, 'leo_delete_bucket_queue'). %% @NEW
-type(mq_id() :: ?QUEUE_ID_PER_OBJECT |
                 ?QUEUE_ID_SYNC_BY_VNODE_ID |
                 ?QUEUE_ID_DIRECTORY |
                 ?QUEUE_ID_REBALANCE |
                 ?QUEUE_ID_ASYNC_DELETION |
                 ?QUEUE_ID_RECOVERY_NODE |
                 ?QUEUE_ID_SYNC_OBJ_WITH_DC |
                 ?QUEUE_ID_COMP_META_WITH_DC |
                 ?QUEUE_ID_REQ_DEL_DIR |
                 ?QUEUE_ID_DEL_DIR).

-define(MSG_PATH_PER_OBJECT,        "1").
-define(MSG_PATH_SYNC_VNODE_ID,     "2").
-define(MSG_PATH_REBALANCE,         "3").
-define(MSG_PATH_ASYNC_DELETION,    "4").
-define(MSG_PATH_RECOVERY_NODE,     "5").
-define(MSG_PATH_SYNC_OBJ_WITH_DC,  "6").
-define(MSG_PATH_COMP_META_WITH_DC, "7").
-define(MSG_PATH_REQ_DEL_DIR,       "11").
-define(MSG_PATH_DEL_DIR,           "12").

-define(ERR_TYPE_REPLICATE_DATA, 'error_msg_replicate_data').
-define(ERR_TYPE_RECOVER_DATA, 'error_msg_recover_data').
-define(ERR_TYPE_DELETE_DATA, 'error_msg_delete_data').
-define(ERR_TYPE_REPLICATE_INDEX, 'error_msg_replicate_index').
-define(ERR_TYPE_RECOVER_INDEX, 'error_msg_recover_index').
-define(ERR_TYPE_DELETE_INDEX, 'error_msg_delete_index').

-define(TBL_REBALANCE_COUNTER, 'leo_rebalance_counter').


%% @doc error messages.
-define(ERROR_COULD_NOT_GET_DATA, "Could not get data").
-define(ERROR_COULD_NOT_GET_META, "Could not get a metadata").
-define(ERROR_COULD_NOT_GET_METADATAS, "Could not get metadatas").
-define(ERROR_COULD_NOT_GET_SYSTEM_CONF,"Could not get the system configuration").
-define(ERROR_RECOVER_FAILURE, "Recover failure").
-define(ERROR_REPLICATE_FAILURE, "Replicate failure").
-define(ERROR_COULD_NOT_GET_REDUNDANCY, "Could not get redundancy").
-define(ERROR_COULD_NOT_CONNECT, "Could not connect").
-define(ERROR_COULD_NOT_MATCH, "Could not match").
-define(ERROR_COULD_SEND_OBJ, "Could not send an object to a remote cluster").
-define(ERROR_NOT_SATISFY_QUORUM, "Could not satisfy the quorum of the consistency level").
-define(ERROR_SYSTEM_HIGH_LOAD, "System High load").
-define(ERROR_COULD_NOT_UPDATE_LOG_LEVEL, "Could not update a log-level").
-define(ERROR_ENQUEUE_FAILURE, "Enqueue failure").


%% @doc notified message items
%-define(MSG_ITEM_TIMEOUT, 'timeout').
%-define(MSG_ITEM_SLOW_OP, 'slow_op').


%% @doc request parameter for READ
-record(read_parameter, {
          ref :: reference(),
          addr_id = 0 :: non_neg_integer(),
          key = <<>> :: binary(),
          etag = 0 :: non_neg_integer(),
          start_pos = -1 :: integer(),
          end_pos   = -1 :: integer(),
          quorum = 0 :: non_neg_integer(),
          req_id = 0 :: non_neg_integer()
         }).

%% @doc Queue's Message.
-record(inconsistent_data_message, {
          id = 0 :: non_neg_integer(),
          type :: atom(),
          addr_id = 0 :: non_neg_integer(),
          key :: any(),
          meta :: tuple(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).

-record(inconsistent_data_message_1, {
          id = 0 :: non_neg_integer(),
          type :: atom(),
          addr_id = 0 :: non_neg_integer(),
          key :: any(),
          meta :: tuple(),
          sync_node :: atom(),
          is_force_sync = false :: boolean(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).
-define(MSG_INCONSISTENT_DATA, 'inconsistent_data_message_1').
-define(transform_inconsistent_data_message(_Msg),
        begin
            case _Msg of
                #inconsistent_data_message{id = _Id,
                                           type = _Type,
                                           addr_id = _AddrId,
                                           key = _Key,
                                           meta = _Meta,
                                           timestamp = _Timestamp,
                                           times = _Time} ->
                    {ok, #inconsistent_data_message_1{
                            id = _Id,
                            type = _Type,
                            addr_id = _AddrId,
                            key = _Key,
                            meta = _Meta,
                            is_force_sync = false,
                            timestamp = _Timestamp,
                            times = _Time}};
                #inconsistent_data_message_1{} ->
                    {ok,_Msg};
                _ ->
                    {error, invalid_record}
            end
        end).

-record(inconsistent_index_message, {
          id = 0 :: non_neg_integer(),
          type :: atom(),
          addr_id = 0 :: non_neg_integer(),
          key :: any(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).

-record(sync_unit_of_vnode_message, {
          id = 0 :: non_neg_integer(),
          vnode_id = 0 :: non_neg_integer(),
          node :: atom(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()
         }).

-record(rebalance_message, {
          id = 0 :: non_neg_integer(),
          vnode_id = 0 :: non_neg_integer(),
          addr_id = 0 :: non_neg_integer(),
          key = <<>> :: binary(),
          node :: atom(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()
         }).

-record(async_deletion_message, {
          id = 0 :: non_neg_integer(),
          addr_id = 0 :: non_neg_integer(),
          key :: any(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).
-record(async_deletion_message_1, {
          id = 0 :: non_neg_integer(),
          addr_id = 0 :: non_neg_integer(),
          key :: any(),
          meta :: term(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).
-define(MSG_ASYNC_DELETION, 'async_deletion_message_1').
-define(transform_async_deletion_message(_Msg),
        begin
            case _Msg of
                #async_deletion_message{id = _Id,
                                        addr_id = _AddrId,
                                        key = _Key,
                                        timestamp = _Timestamp,
                                        times = _Time} ->
                    {ok, #async_deletion_message_1{
                            id = _Id,
                            addr_id = _AddrId,
                            key = _Key,
                            timestamp = _Timestamp,
                            times = _Time}};
                #async_deletion_message_1{} ->
                    {ok,_Msg};
                _ ->
                    {error, invalid_record}
            end
        end).


-record(recovery_node_message, {
          id = 0 :: non_neg_integer(),
          node :: atom(),
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).

-record(inconsistent_data_with_dc, {
          id = 0 :: non_neg_integer(),
          cluster_id :: atom(),
          addr_id = 0 :: non_neg_integer(),
          key = <<>> :: binary(),
          del = 0 :: non_neg_integer(), %% del:[0:false, 1:true]
          timestamp = 0 :: non_neg_integer(),
          times = 0 :: non_neg_integer()}).

-record(comparison_metadata_with_dc, {
          id = 0 :: non_neg_integer(),
          cluster_id :: atom(),
          list_of_addrid_and_key :: list(),
          timestamp = 0 :: non_neg_integer()
         }).

-record(delete_dir, {
          id = 0 :: non_neg_integer(),
          node :: atom(),
          dir = <<>> :: binary(),
          timestamp = 0 :: non_neg_integer()
         }).


%% @doc macros.
-define(env_manager_nodes(),
        case application:get_env(leo_storage, managers) of
            {ok, EnvManagerNodes} ->
                EnvManagerNodes;
            _ ->
                []
        end).

-define(env_storage_device(),
        case application:get_env(leo_storage, obj_containers) of
            {ok, EnvStorageDevice} ->
                EnvStorageDevice;
            _ ->
                []
        end).

-define(env_del_dir_state_dir(),
        case application:get_env(leo_storage, queue_dir) of
            {ok,_EnvQueueDir} ->
                filename:join([_EnvQueueDir, "del_dir", "state"]);
            _ ->
                []
        end).

-define(env_mq_backend_db(),
        case application:get_env(leo_storage, mq_backend_db) of
            {ok, EnvMQBackendDB} ->
                EnvMQBackendDB;
            _ ->
                'leveldb'
        end).

-define(env_num_of_mq_procs(),
        case application:get_env(leo_storage, num_of_mq_procs) of
            {ok, NumOfMQProcs} -> NumOfMQProcs;
            _ -> 3
        end).

%% [MQ.interval beween batch processes]
-define(env_mq_interval_between_batch_procs_min(),
        case application:get_env(leo_storage, mq_interval_between_batch_procs_min) of
            {ok, MQWaitingTimeMin} -> MQWaitingTimeMin;
            _ -> 10
        end).
-define(env_mq_interval_between_batch_procs_max(),
        case application:get_env(leo_storage, mq_interval_between_batch_procs_max) of
            {ok, MQWaitingTimeMax} -> MQWaitingTimeMax;
            _ -> 1000
        end).

-define(env_mq_interval_between_batch_procs_reg(),
        case application:get_env(leo_storage, mq_interval_between_batch_procs_reg) of
            {ok, MQWaitingTimeReg} -> MQWaitingTimeReg;
            _ -> 100
        end).

-define(env_mq_interval_between_batch_procs_step(),
        case application:get_env(leo_storage, mq_interval_between_batch_procs_step) of
            {ok, MQWaitingTimeMax} -> MQWaitingTimeMax;
            _ -> 100
        end).

%% [MQ.batch prcoc of messages]
-define(env_mq_num_of_batch_process_min(),
        case application:get_env(leo_storage, mq_num_of_batch_process_min) of
            {ok, MQBatchProcsMin} -> MQBatchProcsMin;
            _ -> 100
        end).
-define(env_mq_num_of_batch_process_max(),
        case application:get_env(leo_storage, mq_num_of_batch_process_max) of
            {ok, MQBatchProcsMax} -> MQBatchProcsMax;
            _ -> 10000
        end).
-define(env_mq_num_of_batch_process_reg(),
        case application:get_env(leo_storage, mq_num_of_batch_process_reg) of
            {ok, MQBatchProcsReg} -> MQBatchProcsReg;
            _ -> 5000
        end).
-define(env_mq_num_of_batch_process_step(),
        case application:get_env(leo_storage, mq_num_of_batch_process_step) of
            {ok, MQBatchProcsStep} -> MQBatchProcsStep;
            _ -> 1000
        end).


-define(env_size_of_stacked_objs(),
        case application:get_env(leo_storage, size_of_stacked_objs) of
            {ok, SizeOfStackedObjs} -> SizeOfStackedObjs;
            _ -> (1024 * 1024) %% 1MB
        end).

-define(env_stacking_timeout(),
        case application:get_env(leo_storage, stacking_timeout) of
            {ok, StackingTimeout} -> timer:seconds(StackingTimeout);
            _ -> 1000 %% 1sec
        end).

-define(env_grp_level_1(),
        case application:get_env(leo_storage, grp_level_1) of
            {ok, _GrpLevel1} -> _GrpLevel1;
            _ -> []
        end).

-define(env_grp_level_2(),
        case application:get_env(leo_storage, grp_level_2) of
            {ok, _GrpLevel2} -> _GrpLevel2;
            _ -> []
        end).

-define(env_num_of_vnodes(),
        case application:get_env(leo_storage, num_of_vnodes) of
            {ok, _NumOfVNodes} -> _NumOfVNodes;
            _ -> 168
        end).

-define(env_del_dir_workers(),
        case application:get_env(leo_storage, mq_num_of_del_dir_workers) of
            {ok, EnvDelDirWorkers} ->
                EnvDelDirWorkers;
            _ ->
                ?DEF_NUM_OF_DEL_DIR_WORKERS
        end).


-define(DEF_MQ_NUM_OF_BATCH_PROC, 1).
-define(DEF_MQ_INTERVAL_MAX, 32).
-define(DEF_MQ_INTERVAL_MIN,  8).

%% Retrieve a quorum bv a method
-define(quorum(_Method,_W,_D), case _Method of
                                   ?CMD_PUT    -> _W;
                                   ?CMD_DELETE -> _D;
                                   _ -> _W
                               end).

%% For Multi-DC Replication
-define(DEF_PREFIX_MDCR_SYNC_PROC_1, "leo_mdcr_sync_w1_").
-define(DEF_PREFIX_MDCR_SYNC_PROC_2, "leo_mdcr_sync_w2_").
-define(DEF_MDCR_SYNC_PROC_BUFSIZE, 1024 * 1024 * 32).  %% 32MB
-define(DEF_MDCR_SYNC_PROC_TIMEOUT, timer:seconds(30)). %% 30sec
-define(DEF_MDCR_REQ_TIMEOUT,       timer:seconds(30)). %% 30sec
-define(DEF_MDCR_SYNC_PROCS, 1).
-define(DEF_RPC_LISTEN_PORT, 13075).
-define(DEF_MAX_RETRY_TIMES, 3).

-define(DEF_BIN_CID_SIZE,  16).     %% clusterid-size
-define(DEF_BIN_META_SIZE, 16).     %% metadata-size
-define(DEF_BIN_OBJ_SIZE,  32).     %% object-size
-define(DEF_BIN_PADDING, <<0:64>>). %% footer

-ifdef(TEST).
-define(env_mdcr_sync_proc_buf_size(), 1024).
-define(env_mdcr_sync_proc_timeout(),    30).
-define(env_mdcr_req_timeout(),       30000).
-define(env_num_of_mdcr_sync_procs(),     1).
-define(env_rpc_port(), ?DEF_RPC_LISTEN_PORT).

-else.
-define(env_mdcr_sync_proc_buf_size(),
        case application:get_env(leo_storage, mdcr_size_of_stacked_objs) of
            {ok, _MDCRSyncProcBufSize} -> _MDCRSyncProcBufSize;
            _ -> ?DEF_MDCR_SYNC_PROC_BUFSIZE
        end).
-define(env_mdcr_sync_proc_timeout(),
        case application:get_env(leo_storage, mdcr_stacking_timeout) of
            {ok, _MDCRSyncProcTimeout} ->  timer:seconds(_MDCRSyncProcTimeout);
            _ -> ?DEF_MDCR_SYNC_PROC_TIMEOUT
        end).
-define(env_mdcr_req_timeout(),
        case application:get_env(leo_storage, mdcr_req_timeout) of
            {ok, _MDCRReqTimeout} -> _MDCRReqTimeout;
            _ -> ?DEF_MDCR_REQ_TIMEOUT
        end).
-define(env_num_of_mdcr_sync_procs(),
        case application:get_env(leo_storage, mdcr_stacking_procs) of
            {ok, _NumOfMDCRSyncProcs} -> _NumOfMDCRSyncProcs;
            _ -> ?DEF_MDCR_SYNC_PROCS
        end).
-define(env_rpc_port(),
        case application:get_env(leo_rpc, listen_port) of
            {ok, _ListenPort} -> _ListenPort;
            _ -> ?DEF_RPC_LISTEN_PORT
        end).
-endif.


-define(mq_id_and_alias, [{leo_delete_dir_queue,        "requests of removing directories"},
                          {leo_req_delete_dir_queue,    "requests of removing directories"},
                          {leo_comp_meta_with_dc_queue, "requests of comparing metadata w/remote-node"},
                          {leo_sync_obj_with_dc_queue,  "requests of synchronizing objects w/remote-node"},
                          {leo_recovery_node_queue,     "requests of recovering objects of the node (incl. recover-consistency)"},
                          {leo_async_deletion_queue,    "requests of removing objects asynchronously"},
                          {leo_rebalance_queue,         "requests of relocating objects"},
                          {leo_sync_by_vnode_id_queue,  "requests of synchronizing objects by vnode-id"},
                          {leo_per_object_queue,        "requests of fixing inconsistency of objects"}]).


%% @doc Storage watchdog related
%%
%% Threshold active size ratio:
%%    * round(active-size/total-size)
%%    * default:50%
-ifdef(TEST).
-define(DEF_WARN_ACTIVE_SIZE_RATIO,      95).
-define(DEF_THRESHOLD_ACTIVE_SIZE_RATIO, 90).
-define(DEF_THRESHOLD_NUM_OF_NOTIFIED_MSGS, 10).
-define(DEF_STORAGE_WATCHDOG_INTERVAL, timer:seconds(3)).
-else.
-define(DEF_WARN_ACTIVE_SIZE_RATIO,      55).
-define(DEF_THRESHOLD_ACTIVE_SIZE_RATIO, 50).
-define(DEF_THRESHOLD_NUM_OF_NOTIFIED_MSGS, 10).
-define(DEF_STORAGE_WATCHDOG_INTERVAL, timer:seconds(180)).
-endif.

-define(WD_ITEM_ACTIVE_SIZE_RATIO, 'active_size_ratio').
-define(WD_ITEM_NOTIFIED_MSGS, 'notified_msgs').
-define(WD_EXCLUDE_ITEMS, ['leo_storage_watchdog_fragment', 'leo_watchdog_cluster']).
-define(DEF_MAX_COMPACTION_PROCS, 1).
-define(DEF_AUTOCOMPACTION_INTERVAL, 3600). %% 3600sec (60min)

%% @doc for auto-compaction:
%%      <a number of data-compaction nodes at the same time>
%%          = <a active number of nodes> x <coefficient>
%%  -   high: 0.1
%%  - middle: 0.075
%%  -    low: 0.05
-define(DEF_COMPACTION_COEFFICIENT_MID, 0.075).

-define(env_warn_active_size_ratio(),
        case application:get_env(leo_storage, warn_active_size_ratio) of
            {ok, EnvWarnActiveSizeRatio} ->
                EnvWarnActiveSizeRatio;
            _ ->
                ?DEF_WARN_ACTIVE_SIZE_RATIO
        end).

-define(env_threshold_active_size_ratio(),
        case application:get_env(leo_storage, threshold_active_size_ratio) of
            {ok, EnvThresholdActiveSizeRatio} ->
                EnvThresholdActiveSizeRatio;
            _ ->
                ?DEF_THRESHOLD_ACTIVE_SIZE_RATIO
        end).

-define(env_threshold_num_of_notified_msgs(),
        case application:get_env(leo_storage, threshold_num_of_notified_msgs) of
            {ok, EnvThresholdNumOfNotifiedMsgs} ->
                EnvThresholdNumOfNotifiedMsgs;
            _ ->
                ?DEF_THRESHOLD_NUM_OF_NOTIFIED_MSGS
        end).

-define(env_storage_watchdog_interval(),
        begin
            _Time = erlang:phash2(erlang:node(), ?DEF_STORAGE_WATCHDOG_INTERVAL),
            case (_Time < timer:seconds(60)) of
                true ->
                    ?DEF_STORAGE_WATCHDOG_INTERVAL
                        - erlang:phash2(erlang:node(), timer:seconds(30));
                false ->
                    _Time
            end
        end).
-define(env_storage_watchdog_msgs_enabled(),
        case application:get_env(leo_storage, watchdog_msgs_enabled) of
            {ok, EnvWathdogMsgEnabled} ->
                EnvWathdogMsgEnabled;
            _ ->
                false
        end).

%% @doc Storage autonomic-operation related
-define(env_auto_compaction_enabled(),
        case application:get_env(leo_storage, auto_compaction_enabled) of
            {ok, EnvAutoCompactionEnabled} ->
                EnvAutoCompactionEnabled;
            _ ->
                false
        end).

-define(env_auto_compaction_parallel_procs(),
        case application:get_env(leo_storage, auto_compaction_parallel_procs) of
            {ok, EnvAutoCompactionParallelProcs} ->
                EnvAutoCompactionParallelProcs;
            _ ->
                ?DEF_MAX_COMPACTION_PROCS
        end).

-define(env_auto_compaction_interval(),
        case application:get_env(leo_storage, auto_compaction_interval) of
            {ok, EnvAutoCompactionInterval} ->
                EnvAutoCompactionInterval;
            _ ->
                ?DEF_AUTOCOMPACTION_INTERVAL
        end).

-define(env_auto_compaction_coefficient(),
        case application:get_env(leo_storage, auto_compaction_coefficient) of
            {ok, EnvAutoCompactionCoefficient} ->
                EnvAutoCompactionCoefficient;
            _ ->
                ?DEF_COMPACTION_COEFFICIENT_MID
        end).


%% @doc Misc
-define(DEF_SEEKING_METADATA_TIMEOUT, 10). %% default:50ms
-define(env_seeking_timeout_per_metadata(),
        case application:get_env(leo_storage, seeking_timeout_per_metadata) of
            {ok, EnvSeekingMetadataTimeout} ->
                EnvSeekingMetadataTimeout;
            _ ->
                ?DEF_SEEKING_METADATA_TIMEOUT
        end).

%% @doc Maximum number of processes for the write operation
-define(DEF_MAX_NUM_OF_PROCS, 3000).
-define(env_max_num_of_procs(),
        case application:get_env(leo_storage, max_num_of_procs) of
            {ok, EnvMaxNumOfProcs} ->
                EnvMaxNumOfProcs;
            _ ->
                ?DEF_MAX_NUM_OF_PROCS
        end).

%%----------------------------------------------------------------------
%% FOR ACCESS-LOG
%%----------------------------------------------------------------------
%% access-log
-define(LOG_GROUP_ID_ACCESS, 'log_grp_access_log').
-define(LOG_ID_ACCESS, 'log_id_access_log').
-define(LOG_FILENAME_ACCESS, "access").

-define(ACC_LOG_L_OK, 0).
-define(ACC_LOG_L_ERROR, 1).

-define(env_access_log_level(),
        case application:get_env(leo_storage, access_log_level) of
            {ok, EnvAccessLogLevel} ->
                EnvAccessLogLevel;
            _ ->
                ?ACC_LOG_L_OK
        end).

-define(can_output_access_log(_Case),
        case _Case of
            ?ACC_LOG_L_OK ->
                true;
            _ ->
                ?env_access_log_level() == ?ACC_LOG_L_ERROR
        end).

-define(access_log_get(_Key,_Size,_ReqId,_Begin,_Msg),
        ?access_log_get(?ACC_LOG_L_OK,_Key,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_get(_Case,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[GET]\t[Gateway]\t~s\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_storage_get(_Key,_Size,_Begin,_Msg),
        ?access_log_storage_get(?ACC_LOG_L_OK,_Key,_Size,_Begin,_Msg)).
-define(access_log_storage_get(_Case,_Key,_Size,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[GET]\t[Storage]\t~s\t~w\t\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_range_get(_Key,_Start,_End,_Size,_ReqId,_Begin,_Msg),
        ?access_log_range_get(?ACC_LOG_L_OK,_Key,_Start,_End,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_range_get(_Case,_Key,_Start,_End,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[GET]\t[Gateway]\t~s[~w-~w]\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Start,
                                                _End,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_storage_put(_Method,_Key,_Size,_ReqId,_Begin,_Msg),
        ?access_log_storage_put(?ACC_LOG_L_OK,_Method,_Key,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_storage_put(_Case,_Method,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case _Method of
                ?CMD_DELETE ->
                    ?access_log_storage_delete(_Case,_Key,_Size,_ReqId,_Begin,_Msg);
                ?CMD_PUT ->
                    ?access_log_storage_put_1(_Case,_Key,_Size,_ReqId,_Begin,_Msg)
            end
        end).

-define(access_log_storage_delete(_Key,_Size,_ReqId,_Begin,_Msg),
        ?access_log_storage_delete(?ACC_LOG_L_OK,_Key,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_storage_delete(_Case,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[DEL]\t[Storage]\t~s\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_storage_put_1(_Case,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[PUT]\t[Storage]\t~s\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_put(_Key,_Size,_ReqId,_Begin,_Msg),
        ?access_log_put(?ACC_LOG_L_OK,_Key,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_put(_Case,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[PUT]\t[Gateway]\t~s\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).

-define(access_log_delete(_Key,_Size,_ReqId,_Begin,_Msg),
        ?access_log_delete(?ACC_LOG_L_OK,_Key,_Size,_ReqId,_Begin,_Msg)).
-define(access_log_delete(_Case,_Key,_Size,_ReqId,_Begin,_Msg),
        begin
            case ?can_output_access_log(_Case) of
                true ->
                    _Latency = erlang:round((leo_date:clock() - _Begin) / 1000),
                    leo_logger_api:append(
                      {?LOG_ID_ACCESS,
                       #message_log{ format  = "[DEL]\t[Gateway]\t~s\t~w\t~w\t~s\t~w\t~w\t~p\n",
                                     message = [_Key,
                                                _Size,
                                                _ReqId,
                                                leo_date:date_format(),
                                                leo_date:clock(),
                                                _Latency,
                                                _Msg
                                               ]}
                      });
                false ->
                    ok
            end
        end).


%%----------------------------------------------------------------------
%% FOR DELETION-DIR/BUCKET
%%----------------------------------------------------------------------
-define(DEF_NUM_OF_DEL_DIR_WORKERS, 8).
-define(DEL_DIR_STATE_DB_ID, 'leo_del_dir_state').
-define(DEL_DIR_STATE_DB_ID_STR, atom_to_list(?DEL_DIR_STATE_DB_ID)).
-define(PREFIX_DEL_DIR_MQ, "leo_del_dir_mq_").

-define(STATE_PENDING, 0).
-define(STATE_ENQUEUING, 1).
-define(STATE_MONITORING, 2).
-define(STATE_FINISHED, 3).
-type(del_dir_state() :: ?STATE_PENDING |
                         ?STATE_ENQUEUING |
                         ?STATE_MONITORING |
                         ?STATE_FINISHED).

-define(TYPE_DEL_BUCKET, 0).
-define(TYPE_DEL_DIR, 1).
-type(del_dir_type() :: ?TYPE_DEL_BUCKET |
                        ?TYPE_DEL_DIR).

-define(del_dir_state_to_atom(_DelDirState),
        begin
            case _DelDirState of
                ?STATE_PENDING ->
                    'pending';
                ?STATE_ENQUEUING ->
                    'enqueuing';
                ?STATE_MONITORING ->
                    'monitoring';
                ?STATE_FINISHED ->
                    'finished'
            end
        end).

-record(del_dir_state, {
          mq_id = null :: atom(),
          type = ?TYPE_DEL_BUCKET :: del_dir_type(),
          directory = <<>> :: binary(),
          state = ?STATE_PENDING :: del_dir_state(),
          is_notification_successful = false :: boolean(), %% for del-buclet (communication w/leo_manager)
          enqueued_at = 1 :: pos_integer(),
          timestamp = 1 :: pos_integer()
         }).

-define(del_dir_queue_list(),
        begin
            lists:map(
              fun(N) ->
                      %% Pair: {id, dir}
                      {list_to_atom(
                         lists:append(
                           [atom_to_list(?QUEUE_ID_DEL_DIR), "_", integer_to_list(N)])),
                       lists:append(
                         [?MSG_PATH_DEL_DIR, "/", integer_to_list(N)])}
              end, lists:seq(1, ?env_del_dir_workers()))
        end).

-define(del_dir_id_list(),
        begin
            lists:map(
              fun(N) ->
                      list_to_atom(
                        lists:append(
                          [atom_to_list(?QUEUE_ID_DEL_DIR), "_", integer_to_list(N)]))
              end, lists:seq(1, ?env_del_dir_workers()))
        end).
