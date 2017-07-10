%%====================================================================
%%
%% LeoFS Gateway
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
%%====================================================================
-ifdef(namespaced_types).
-type gw_set() :: sets:set().
-else.
-type gw_set() :: set().
-endif.

%%----------------------------------------------------------------------
%% DEFAULT VALUES
%%----------------------------------------------------------------------
-define(SHUTDOWN_WAITING_TIME, 2000).
-define(S3_HTTP, leo_s3_http).
-define(DEF_LAYERS_OF_DIRS, {1, 64}).

-ifdef(TEST).
-define(DEF_TIMEOUT, 1000).
-define(DEF_REQ_TIMEOUT, 1000).
-else.
-define(DEF_TIMEOUT, 5000). %%  5 sec
-define(DEF_REQ_TIMEOUT, 30000). %% 30 sec
-endif.


%% @pending
%% -define(MSG_INCOMPLETE_BODY,   {400, 'incomplete body'  }).
%% -define(MSG_INVALID_ARGUMENT,  {400, 'invalid argument' }).
%% -define(MSG_INVALID_URI,       {400, 'invalid uri'      }).
%% -define(MSG_KEY_TOO_LONG,      {400, 'key too long'     }).
%% -define(MSG_REQ_TIMEOUT,       {400, 'request timeout'  }).
%% -define(MSG_FILE_NOT_FOUND,    {404, 'file not found'   }).
%% -define(MSG_INTERNAL_ERROR,    {500, 'internal_error'   }).
%% -define(MSG_SLOW_DOWN,         {503, 'slow down'        }).

%% timeout
-define(TIMEOUT_L1_LEN,   65535).
-define(TIMEOUT_L2_LEN,  131071).
-define(TIMEOUT_L3_LEN,  524287).
-define(TIMEOUT_L4_LEN, 1048576).

-define(TIMEOUT_L1_SEC,  5000).
-define(TIMEOUT_L2_SEC,  7000).
-define(TIMEOUT_L3_SEC, 10000).
-define(TIMEOUT_L4_SEC, 20000).
-define(TIMEOUT_L5_SEC, 30000).


%% Interval of the large-object-worker checkout
%% (default: 100ms)
-define(DEF_WAIT_TIME_OF_CHECKOUT, 100).

%% Protocol
-define(PROTO_HANDLER_S3, 'leo_gateway_s3_api').
-define(PROTO_HANDLER_REST, 'leo_gateway_rest_api').
-define(PROTO_HANDLER_EMBED, 'leo_gateway_embed').

-define(PROTO_HANDLER_NFS, 'nfs').
%% -define(HTTP_HANDLER_SWIFT, 'leo_gateway_swift_api').
-define(DEF_PROTOCOL_HANDLER, ?PROTO_HANDLER_S3).

-type(protocol_handler() :: ?PROTO_HANDLER_S3 |
                            ?PROTO_HANDLER_REST |
                            ?PROTO_HANDLER_EMBED |
                            ?PROTO_HANDLER_NFS).
-define(convert_to_handler(_V),
        case _V of
            rest ->
                ?PROTO_HANDLER_REST;
            s3 ->
                ?PROTO_HANDLER_S3;
            embed ->
                ?PROTO_HANDLER_EMBED;
            nfs ->
                ?PROTO_HANDLER_NFS;
            _ ->
                ?PROTO_HANDLER_S3
        end).


%%----------------------------------------------------------------------
%% ERROR MESSAGES
%%----------------------------------------------------------------------
-define(ERROR_COULD_NOT_CONNECT, "Could not connect").
-define(ERROR_NOT_MATCH_LENGTH, "Not match object length").
-define(ERROR_FAIL_PUT_OBJ, "Fail put an object").
-define(ERROR_FAIL_RETRIEVE_OBJ, "Fail retrieve an object").
-define(ERROR_COULD_NOT_START_TRAN, "Could not start a transaction").
-define(ERROR_COULD_NOT_UPDATE_LOG_LEVEL, "Could not update a log level").
-define(ERROR_COULD_NOT_SEND_DISK_CACHE, "Could not send a disk cache").
-define(ERROR_COULD_NOT_OPEN_DISK_CACHE, "Could not open a disk cache").


%%----------------------------------------------------------------------
%% RECORDS
%%----------------------------------------------------------------------
%% large-object
-record(large_obj_info, {key = <<>> :: binary(),
                         length = 0 :: non_neg_integer(),
                         num_of_chunks = 0 :: non_neg_integer(),
                         md5_context = <<>> :: binary()
                        }).


%%----------------------------------------------------------------------
%% MACROS
%%----------------------------------------------------------------------
%% Gateway-related:
-define(env_bucket_prop_sync_interval(),
        case application:get_env(leo_gateway, bucket_prop_sync_interval) of
            {ok, EnvBucketPropSyncInterval} -> EnvBucketPropSyncInterval;
            _ -> 300 %% 300sec/5min
        end).

-define(env_protocol(),
        case application:get_env(leo_gateway, protocol) of
            {ok,_EnvProtocol} -> _EnvProtocol;
            _ -> []
        end).

-define(env_http_properties(),
        case application:get_env(leo_gateway, http) of
            {ok, EnvHttp} -> EnvHttp;
            _ -> []
        end).

-define(env_layer_of_dirs(),
        case application:get_env(leo_gateway, layer_of_dirs) of
            {ok, EnvLayerOfDirs} -> EnvLayerOfDirs;
            _ -> ?DEF_LAYERS_OF_DIRS
        end).

-define(env_listener(),
        case application:get_env(leo_gateway, listener) of
            {ok, EnvListener} -> EnvListener;
            _ -> ?S3_HTTP
        end).

-define(env_cache_properties(),
        case application:get_env(leo_gateway, cache) of
            {ok, EnvCache} -> EnvCache;
            _ -> []
        end).

-define(env_large_object_properties(),
        case application:get_env(leo_gateway, large_object) of
            {ok, EnvLargeObject} -> EnvLargeObject;
            _ -> []
        end).

-define(env_recover_properties(),
        case application:get_env(leo_gateway, recover) of
            {ok, EnvRecover} -> EnvRecover;
            _ -> []
        end).


%% Large size object related definitions
-define(POD_LOH_WORKER, 'pod_loh_worker').
-define(DEF_LOH_PUT_WORKER_POOL_SIZE, 64).
-define(DEF_LOH_PUT_WORKER_BUFFER_SIZE, 32).

-define(env_loh_put_worker_pool_size(),
        case application:get_env(leo_gateway, large_object) of
            {ok, EnvLargeObject_1} ->
                leo_misc:get_value('put_worker_pool_size',
                                   EnvLargeObject_1, ?DEF_LOH_PUT_WORKER_POOL_SIZE);
            _ ->
                ?DEF_LOH_PUT_WORKER_POOL_SIZE
        end).

-define(env_loh_put_worker_buffer_size(),
        case application:get_env(leo_gateway, large_object) of
            {ok, EnvLargeObject_2} ->
                leo_misc:get_value('put_worker_buffer_size',
                                   EnvLargeObject_2, ?DEF_LOH_PUT_WORKER_BUFFER_SIZE);
            _ ->
                ?DEF_LOH_PUT_WORKER_BUFFER_SIZE
        end).


%% Timeout-related:
-define(env_timeout(),
        case application:get_env(leo_gateway, timeout) of
            {ok, EnvTimeout} -> EnvTimeout;
            _ -> []
        end).

-define(env_timeout_level_1(),
        case leo_misc:get_env(leo_gateway, level_1) of
            {ok, EnvTimeoutL1} -> EnvTimeoutL1;
            _ -> ?TIMEOUT_L1_SEC
        end).

-define(env_timeout_level_2(),
        case leo_misc:get_env(leo_gateway, level_2) of
            {ok, EnvTimeoutL2} -> EnvTimeoutL2;
            _ -> ?TIMEOUT_L2_SEC
        end).

-define(env_timeout_level_3(),
        case leo_misc:get_env(leo_gateway, level_3) of
            {ok, EnvTimeoutL3} -> EnvTimeoutL3;
            _ -> ?TIMEOUT_L3_SEC
        end).

-define(env_timeout_level_4(),
        case leo_misc:get_env(leo_gateway, level_4) of
            {ok, EnvTimeoutL4} -> EnvTimeoutL4;
            _ -> ?TIMEOUT_L4_SEC
        end).

-define(env_timeout_level_5(),
        case leo_misc:get_env(leo_gateway, level_5) of
            {ok, EnvTimeoutL5} -> EnvTimeoutL5;
            _ -> ?TIMEOUT_L5_SEC
        end).

-define(env_timeout_for_get(),
        case leo_misc:get_env(leo_gateway, get) of
            {ok, EnvTimeout6} -> EnvTimeout6;
            _ -> ?DEF_REQ_TIMEOUT
        end).

-define(env_timeout_for_ls(),
        case leo_misc:get_env(leo_gateway, ls) of
            {ok, EnvTimeout7} -> EnvTimeout7;
            _ -> ?DEF_REQ_TIMEOUT
        end).

%% QoS related
-define(env_qos_stat_enabled(),
        case leo_misc:get_env(leo_gateway, is_enable_qos_stat) of
            {ok, _IsEnableQoSStat} -> _IsEnableQoSStat;
            _ -> false
        end).
-define(env_qos_notify_enabled(),
        case leo_misc:get_env(leo_gateway, is_enable_qos_notify) of
            {ok, _IsEnableQoSNotify} -> _IsEnableQoSNotify;
            _ -> false
        end).
-define(env_qos_managers(),
        case leo_misc:get_env(savanna_agent, managers) of
            {ok, _SVManagers} -> _SVManagers;
            _ -> []
        end).

%% NFS related
-define(DEF_MOUNTD_PORT, 22050).
-define(DEF_MOUNTD_ACCEPTORS, 128).
-define(DEF_NFSD_PORT, 2049).
-define(DEF_NFSD_ACCEPTORS, 128).
-define(DEF_NFSD_MAX_FILE_SIZE, 18446744073709551615). %% max value in 64bit
-define(DEF_NFSD_RTMAX, 5242880).
-define(DEF_NFSD_WTMAX, 5242880).
-define(DEF_NFSD_READDIR_SCAN_INT, 1800).
-define(DEF_NFSD_READDIR_ENTRY_TTL, 3600).
-define(DEF_NFSD_READDIR_MEM_THRES, 268435456).
-define(DEF_LOCKD_PORT,         22051).
-define(DEF_LOCKD_ACCEPTORS,    128).

-define(env_nfs_options(),
        case application:get_env(leo_gateway, nfs) of
            {ok, _NFS_Options} -> _NFS_Options;
            _ -> []
        end).

%% NLM related
-define(DEF_NLM_HANDLER, leo_nlm_lock_handler_ets).
-define(env_nlm_options(),
        case application:get_env(leo_gateway, nlm) of
            {ok, _NLM_Options} -> _NLM_Options;
            _ -> []
        end).

-record(lock_record,{
          start :: non_neg_integer(),
          till  :: integer(),
          len   :: non_neg_integer(),
          owner :: binary(),
          uppid :: integer(),
          excl  :: boolean()
         }).

-define(NLM_LOCK_ETS, nlm_lock_ets).


%%----------------------------------------------------------------------
%% FOR QoS
%%----------------------------------------------------------------------
-define(QOS_METRIC_BUCKET_SCHEMA, << "leo_bucket" >>).
-define(QOS_METRIC_BUCKET_COL_1, << "cnt_r" >>). %% num of reads
-define(QOS_METRIC_BUCKET_COL_2, << "cnt_w" >>). %% num of writes
-define(QOS_METRIC_BUCKET_COL_3, << "cnt_d" >>). %% num of deletes
-define(QOS_METRIC_BUCKET_COL_4, << "sum_len_r" >>). %% summary of length of an object at read
-define(QOS_METRIC_BUCKET_COL_5, << "sum_len_w" >>). %% summary of length of an object at write
-define(QOS_METRIC_BUCKET_COL_6, << "sum_len_d" >>). %% summary of length of an object at delete
-define(QOS_METRIC_BUCKET_COL_7, << "his_len_r" >>). %% histogram of length of an object at read
-define(QOS_METRIC_BUCKET_COL_8, << "his_len_w" >>). %% histogram of length of an object at write
-define(QOS_METRIC_BUCKET_COL_9, << "his_len_d" >>). %% histogram of length of an object at delete


%%----------------------------------------------------------------------
%% FOR STATISTICS
%%----------------------------------------------------------------------
-record(statistics, {
          id = 0 :: integer(),
          read = 0 :: integer(),
          write = 0 :: integer(),
          delete = 0 :: integer(),
          head = 0 :: integer(),
          pending = 0 :: integer(),
          error = 0 :: integer(),
          total_mem_usage = 0 :: integer(),
          system_mem_usage = 0 :: integer(),
          proc_mem_usage = 0 :: integer(),
          num_of_procs = 0 :: integer()
         }).


%%----------------------------------------------------------------------
%% FOR ACCESS-LOG
%%----------------------------------------------------------------------
%% access-log
-define(LOG_GROUP_ID_ACCESS, 'log_grp_access_log').
-define(LOG_ID_ACCESS, 'log_id_access_log').
-define(LOG_FILENAME_ACCESS, "access").

-define(notify_metrics(_Method,_Bucket,_Size),
        begin
            Cols = case _Method of
                       <<"GET">> ->
                           [?QOS_METRIC_BUCKET_COL_1,
                            ?QOS_METRIC_BUCKET_COL_4,
                            ?QOS_METRIC_BUCKET_COL_7];
                       <<"PUT">> ->
                           [?QOS_METRIC_BUCKET_COL_2,
                            ?QOS_METRIC_BUCKET_COL_5,
                            ?QOS_METRIC_BUCKET_COL_8];
                       <<"DELETE">> ->
                           [?QOS_METRIC_BUCKET_COL_3,
                            ?QOS_METRIC_BUCKET_COL_6,
                            ?QOS_METRIC_BUCKET_COL_9]
                   end,
            leo_gateway_qos_stat:notify(?QOS_METRIC_BUCKET_SCHEMA,
                                        _Bucket, lists:nth(1, Cols), 1),
            leo_gateway_qos_stat:notify(?QOS_METRIC_BUCKET_SCHEMA,
                                        _Bucket, lists:nth(2, Cols),_Size),
            leo_gateway_qos_stat:notify(?QOS_METRIC_BUCKET_SCHEMA,
                                        _Bucket, lists:nth(3, Cols),_Size)
        end).

-define(get_child_num(_Path_1),
        begin
            case string:str(_Path_1, "\n") of
                0 ->
                    {_Path_1, 0};
                _Index ->
                    case catch list_to_integer(string:sub_string(_Path_1, _Index + 1)) of
                        {'EXIT',_} ->
                            {_Path_1, 0};
                        _Num->
                            {string:sub_string(_Path_1, 1, _Index -1), _Num}
                    end
            end
        end).

-define(access_log_get(_Bucket,_Path,_Size,_Response,_Begin),
        begin
            ?access_log_get(_Bucket,_Path,_Size,_Response,_Begin,"miss")
        end).
-define(access_log_get(_Bucket,_Path,_Size,_Response,_Begin,_Cache),
        begin
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[GET]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _OrgPath,
                                       _ChildNum,
                                       _Size,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       _Cache
                                      ]}
              })
            %% ?notify_metrics(<<"GET">>,_Bucket,_Size)
        end).
-define(access_log_get_acl(_Bucket,_Path,_Response,_Begin),
        begin
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[GET-ACL]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _OrgPath,
                                       _ChildNum,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).
-define(access_log_put(_Bucket,_Path,_Size,_Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[PUT]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _OrgPath,
                                       _ChildNum,
                                       _Size,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
            %% ?notify_metrics(<<"PUT">>,_Bucket,_Size)
        end).
-define(access_log_copy(_Bucket,_Path,_Size,_Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[COPY]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _Path,
                                       0,
                                       _Size,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
            %% ?notify_metrics(<<"PUT">>,_Bucket,_Size)
        end).
-define(access_log_delete(_Bucket,_Path,_Size,_Response,_Begin),
        begin
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[DELETE]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _OrgPath,
                                       _ChildNum,
                                       _Size,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
            %% ?notify_metrics(<<"DELETE">>,_Bucket,_Size)
        end).
-define(access_log_head(_Bucket,_Path,_Response,_Begin),
        begin
            {_OrgPath, _ChildNum} = ?get_child_num(binary_to_list(_Path)),
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[HEAD]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _OrgPath,
                                       _ChildNum,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).

-define(access_log(_Method,_Bucket,_Path,_Size,_Response,_Begin),
        begin
            case _Method of
                get ->
                    ?access_log_get(_Bucket,_Path,_Size,_Response,_Begin);
                get_acl ->
                    ?access_log_get_acl(_Bucket,_Path,_Response,_Begin);
                put ->
                    ?access_log_put(_Bucket,_Path,_Size,_Response,_Begin);
                delete ->
                    ?access_log_delete(_Bucket,_Path,_Size,_Response,_Begin);
                head ->
                    ?access_log_head(_Bucket,_Path,_Response,_Begin)
            end
        end).

%% access-log for buckets
-define(access_log_bucket_put(_Bucket,_Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-PUT]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       "",
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
            %% ?notify_metrics(<<"PUT">>,_Bucket,_Size)
        end).
-define(access_log_bucket_delete(_Bucket,_Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-DELETE]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       "",
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
            %% ?notify_metrics(<<"DELETE">>,_Bucket,_Size)
        end).
-define(access_log_bucket_head(_Bucket,_Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-HEAD]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       "",
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).
-define(access_log_bucket_get(_Bucket, _Prefix, _Response,_Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-GET]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       binary_to_list(_Prefix),
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).
-define(access_log_bucket_getacl(_Bucket, _Response, _Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-GETACL]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       "",
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).
-define(access_log_bucket_getacl(_Bucket, _CannedACL, _Response, _Begin),
        begin
            _Clock = leo_date:clock(),
            _Latency = erlang:round((_Clock - _Begin) / 1000),
            leo_logger_client_base:append(
              {?LOG_ID_ACCESS,
               #message_log{format  = "[BUCKET-PUTACL]\t~s\t~s\t~w\t~w\t~s\t~w\t~w\t~w\t~s\n",
                            message = [binary_to_list(_Bucket),
                                       _CannedACL,
                                       0,
                                       0,
                                       leo_date:date_format(),
                                       _Clock,
                                       _Response,
                                       _Latency,
                                       ""
                                      ]}
              })
        end).

-define(reply_fun(_Cause,_Method,_Bucket,_Key,_Len,_Begin),
        case _Cause of
            not_found ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_NOT_FOUND,_Begin);
            unavailable ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_SERVICE_UNAVAILABLE,_Begin);
            timeout ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_GATEWAY_TIMEOUT,_Begin);
            bad_range ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_BAD_RANGE,_Begin);
            _ ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_INTERNAL_ERROR,_Begin)
        end).

-define(reply_fun(_Cause,_Method,_Bucket,_Key,_Len,_Req,_Begin),
        case _Cause of
            not_found ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_NOT_FOUND,_Begin),
                case _Method of
                    delete ->
                        ?reply_no_content([?SERVER_HEADER], Req);
                    head ->
                        ?reply_not_found_without_body([?SERVER_HEADER], Req);
                    _ ->
                        ?reply_not_found([?SERVER_HEADER],_Key, <<>>,_Req)
                end;
            unavailable ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_SERVICE_UNAVAILABLE,_Begin),
                ?reply_service_unavailable_error([?SERVER_HEADER],_Key, <<>>,_Req);
            timeout ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_GATEWAY_TIMEOUT,_Begin),
                ?reply_timeout([?SERVER_HEADER],_Key, <<>>,_Req);
            bad_range ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_BAD_RANGE,_Begin),
                ?reply_bad_range([?SERVER_HEADER], Key, <<>>, Req);
            _ ->
                ?access_log(_Method,_Bucket,_Key,_Len, ?HTTP_ST_INTERNAL_ERROR,_Begin),
                ?reply_internal_error([?SERVER_HEADER],_Key, <<>>,_Req)
        end).
