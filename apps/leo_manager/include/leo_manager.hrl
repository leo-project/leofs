%%====================================================================
%%
%% Leo Manager
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
-include_lib("eunit/include/eunit.hrl").

-ifdef(namespaced_types).
-type mgr_dict() :: dict:dict().
-else.
-type mgr_dict() :: dict().
-endif.


%% constants
-define(SHUTDOWN_WAITING_TIME, 2000).
-define(MAX_RESTART, 5).
-define(MAX_TIME, 60).
-define(RETRY_TIMES, 5).

-ifdef(TEST).
-define(DEF_TIMEOUT, 1000).           %% 1sec
-define(DEF_MONITOR_INTERVAL, 3000).  %% 3sec
-define(DEF_PROC_INTERVAL, 100).      %% 100ms
-define(DEF_RING_SYNC_INTERVAL, 100). %% 100ms
-else.
-define(DEF_TIMEOUT, 120000).           %% 120sec
-define(DEF_MONITOR_INTERVAL, 20000).   %%  20sec
-define(DEF_PROC_INTERVAL, 250).        %% 250ms
-define(DEF_RING_SYNC_INTERVAL, 10000). %%  10sec
-endif.

-define(TIMEOUT_FOR_LEOFSADM, 5000). %% 5sec

-define(SYSTEM_CONF_FILE, "conf/leofs.conf").


-ifdef(TEST).
-define(CURRENT_TIME, 65432100000).
-define(APPLY_AFTER_TIME, 0).
-else.
-define(CURRENT_TIME, leo_date:now()).
-define(APPLY_AFTER_TIME, 200).
-endif.


%% manager-related tables
-define(TBL_STORAGE_NODES, 'leo_storage_nodes').
-define(TBL_GATEWAY_NODES, 'leo_gateway_nodes').
-define(TBL_REBALANCE_INFO, 'leo_rebalance_info').
-define(TBL_HISTORIES, 'leo_histories').
-define(TBL_AVAILABLE_CMDS, 'leo_available_commands').
-define(TBL_DEL_BUCKET_STATE, 'leo_del_bucket_state').

%% server-type
-define(SERVER_TYPE_STORAGE, "S").
-define(SERVER_TYPE_GATEWAY, "G").


%% command-related
-define(COMMAND_ERROR, "Command Error").
-define(COMMAND_DELIMITER, " \r\n").

-define(OK, "OK\r\n").
-define(ERROR, "ERROR\r\n").
-define(LF, "\n").
-define(CRLF, "\r\n").
-define(SPACE, " ").
-define(SEPARATOR, " | ").
-define(STORED, "STORED\r\n").
-define(NOT_STORED, "NOT_STORED\r\n").
-define(DELETED, "DELETED\r\n").
-define(NOT_FOUND, "NOT FOUND\r\n").
-define(SERVER_ERROR, "SERVER_ERROR").
-define(BYE, "BYE\r\n").

%% Common Commands
-define(CMD_HELP, "help").
-define(CMD_QUIT, "quit").
-define(CMD_VERSION, "version").
-define(CMD_STATUS, "status").

%% For S3-API
-define(CMD_CREATE_USER, "create-user").
-define(CMD_IMPORT_USER, "import-user").
-define(CMD_UPDATE_USER_ROLE, "update-user-role").
-define(CMD_UPDATE_USER_PW, "update-user-password").
-define(CMD_DELETE_USER, "delete-user").
-define(CMD_GET_USERS, "get-users").
-define(CMD_ADD_ENDPOINT, "add-endpoint").
-define(CMD_SET_ENDPOINT, "set-endpoint").
-define(CMD_DEL_ENDPOINT, "delete-endpoint").
-define(CMD_GET_ENDPOINTS, "get-endpoints").
-define(CMD_ADD_BUCKET, "add-bucket").
-define(CMD_GET_BUCKETS, "get-buckets").
-define(CMD_GET_BUCKET_BY_ACCESS_KEY, "get-bucket").
-define(CMD_SET_RED_METHOD, "set-redundancy-method").
-define(CMD_DELETE_BUCKET, "delete-bucket").
-define(CMD_DELETE_BUCKET_STATS, "delete-bucket-stats").
-define(CMD_RESET_DELETE_BUCKET_STATS, "reset-delete-bucket-stats").
-define(CMD_CHANGE_BUCKET_OWNER, "chown-bucket").
-define(CMD_UPDATE_ACL, "update-acl").

%% For Storage
-define(CMD_ATTACH, "attach").
-define(CMD_DETACH, "detach").
-define(CMD_SUSPEND, "suspend").
-define(CMD_RESUME, "resume").
-define(CMD_ROLLBACK, "rollback").
-define(CMD_START, "start").
-define(CMD_REBALANCE, "rebalance").
-define(CMD_COMPACT, "compact").
-define(CMD_DIAGNOSE_DATA, "diagnose-data").
-define(CMD_DU, "du").
-define(CMD_WHEREIS, "whereis").
%% For Storage MQ
-define(CMD_MQ_STATS, "mq-stats").
-define(CMD_MQ_SUSPEND, "mq-suspend").
-define(CMD_MQ_RESUME, "mq-resume").

%% For Gateway
-define(CMD_PURGE, "purge").
-define(CMD_REMOVE, "remove").
-define(CMD_BACKUP_MNESIA, "backup-mnesia").
-define(CMD_RESTORE_MNESIA, "restore-mnesia").
-define(CMD_UPDATE_MANAGERS, "update-managers").
%% For Maintenance
-define(CMD_RECOVER, "recover").
-define(CMD_DUMP_RING, "dump-ring").
-define(CMD_UPDATE_LOG_LEVEL, "update-log-level").
-define(CMD_UPDATE_CONSISTENCY_LEVEL, "update-consistency-level").
-define(CMD_UPDATE_PROP, "update-property").
-define(CMD_GEN_NFS_MNT_KEY, "gen-nfs-mnt-key").
%% For MDC-Replication
-define(CMD_JOIN_CLUSTER, "join-cluster").
-define(CMD_REMOVE_CLUSTER, "remove-cluster").
-define(CMD_CLUSTER_STAT, "cluster-status").

-define(LOGIN, "login").
-define(AUTHORIZED, <<"_authorized_\r\n">>).
-define(USER_ID, <<"_user_id_\r\n">>).
-define(PASSWORD, <<"_password_\r\n">>).

-define(COMMANDS, [{?CMD_HELP, "help"},
                   {?CMD_QUIT, "quit"},
                   {?CMD_VERSION, "version"},
                   {?CMD_STATUS,  "status [<storage-node>|<gateway-node>]"},
                   {?CMD_DUMP_RING, "dump-ring <manager-node>|<storage-node>|<gateway-node>"},
                   {?CMD_UPDATE_LOG_LEVEL, "update-log-level (<storage-node>|<gateway-node>) (debug|info|warn|error)"},
                   {?CMD_UPDATE_CONSISTENCY_LEVEL, "update-consistency-level (<storage-node>|<gateway-node>) <write-quorum> <read-quorum> <delete-quorum>"},
                   %% for Cluster
                   {?CMD_WHEREIS, "whereis <path>"},
                   {?CMD_RECOVER, lists:append(
                                    ["recover file <path>", ?CRLF,
                                     "recover dir [<path>]", ?CRLF,
                                     "recover disk <storage-node> <disk-id>", ?CRLF,
                                     "recover consistency <storage-node>", ?CRLF,
                                     "recover node <storage-node>", ?CRLF,
                                     "recover ring <storage-node>", ?CRLF,
                                     "recover cluster <cluster-id>"
                                    ])},
                   {?CMD_DETACH, "detach <storage-node>"},
                   {?CMD_SUSPEND, "suspend <storage-node>"},
                   {?CMD_RESUME, "resume <storage-node>"},
                   {?CMD_ROLLBACK, "rollback <storage-node>"},
                   {?CMD_START, "start"},
                   {?CMD_REBALANCE, "rebalance"},
                   %% for Storage
                   {?CMD_COMPACT, lists:append(
                                    ["compact start <storage-node> all|<num_of_targets> [<num_of_compact_procs>]", ?CRLF,
                                     "compact suspend <storage-node>", ?CRLF,
                                     "compact resume  <storage-node>", ?CRLF,
                                     "compact status  <storage-node>"
                                    ])},
                   {?CMD_DIAGNOSE_DATA, "diagnose-data <storage-node>"},
                   {?CMD_DU, "du <storage-node>"},
                   %% for Storage MQ
                   {?CMD_MQ_STATS, "mq-stats <storage-node>"},
                   {?CMD_MQ_SUSPEND, "mq-suspend <storage-node> <mq-id>"},
                   {?CMD_MQ_RESUME, "mq-resume <storage-node> <mq-id>"},
                   %% for Gateway
                   {?CMD_PURGE, "purge <path>"},
                   {?CMD_REMOVE, "remove <gateway-node>"},
                   %% for Watchdog
                   {?CMD_UPDATE_PROP, "update-property <node> <property-name> <property-value>"},
                   %% for S3-API
                   %% - user-related
                   {?CMD_CREATE_USER, "create-user <user-id> [<password>]"},
                   {?CMD_IMPORT_USER, "import-user <user-id> <access-key-id> <secret-access-key>"},
                   {?CMD_DELETE_USER, "delete-user <user-id>"},
                   {?CMD_UPDATE_USER_ROLE, "update-user-role <user-id> <role-id>"},
                   {?CMD_UPDATE_USER_PW, "update-user-password <user-id> <password>"},
                   {?CMD_GET_USERS, "get-users"},
                   %% - endpoint-related
                   {?CMD_ADD_ENDPOINT, "add-endpoint <endpoint>"},
                   {?CMD_SET_ENDPOINT, "set-endpoint <endpoint>"},
                   {?CMD_DEL_ENDPOINT, "delete-endpoint <endpoint>"},
                   {?CMD_GET_ENDPOINTS, "get-endpoints"},
                   %% - bucket-related
                   {?CMD_ADD_BUCKET, "add-bucket <bucket> <access-key-id>"},
                   {?CMD_DELETE_BUCKET, "delete-bucket <bucket> <access-key-id>"},
                   {?CMD_DELETE_BUCKET_STATS, "delete-bucket-stats <bucket>"},
                   {?CMD_RESET_DELETE_BUCKET_STATS, "reset-delete-bucket-stats <bucket>"},
                   {?CMD_GET_BUCKETS, "get-buckets"},
                   {?CMD_GET_BUCKET_BY_ACCESS_KEY, "get-bucket <access-key-id>"},
                   {?CMD_CHANGE_BUCKET_OWNER, "chown-bucket <bucket> <new-access-key-id>"},
                   {?CMD_SET_RED_METHOD, "set-redundancy-method <bucket> <access-key-id> <redundancy-method>"},
                   {?CMD_GEN_NFS_MNT_KEY, "gen-nfs-mnt-key <bucket> <access-key-id> <client-ip-address>"},
                   %% - acl-related
                   {?CMD_UPDATE_ACL, "update-acl <bucket> <access-key-id> private|public-read|public-read-write"},
                   %% - multi-dc replication
                   {?CMD_JOIN_CLUSTER, "join-cluster <remote-manager-master> <remote-manager-slave>"},
                   {?CMD_REMOVE_CLUSTER, "remove-cluster <remote-manager-master> <remote-manager-slave>"},
                   {?CMD_CLUSTER_STAT, "cluster-status"},
                   %% for Manager
                   {?CMD_UPDATE_MANAGERS, "update-managers <manager-master> <manager-slave>"},
                   {?CMD_BACKUP_MNESIA, "backup-mnesia <backupfilepath>"},
                   {?CMD_RESTORE_MNESIA, "restore-mnesia <backupfilepath>"}
                  ]).
-record(cmd_state, {name :: string(),
                    help :: string(),
                    available = true :: boolean()
                   }).

%% du-command-related
-define(NULL_DATETIME, "____-__-__ __:__:__").

%% compaction-related
-define(COMPACT_START, "start").
-define(COMPACT_SUSPEND, "suspend").
-define(COMPACT_RESUME, "resume").
-define(COMPACT_STATUS, "status").
-define(COMPACT_TARGET_ALL, "all").

%% recover type
-define(RECOVER_FILE, "file").
-define(RECOVER_DISK, "disk").
-define(RECOVER_CONSISTENCY, "consistency").
-define(RECOVER_NODE, "node").
-define(RECOVER_RING, "ring").
-define(RECOVER_REMOTE_CLUSTER, "cluster").
-define(RECOVER_DIR, "dir").

%% membership
-define(DEF_NUM_OF_ERROR_COUNT, 2).

%% error
-define(ERROR_COULD_NOT_CONNECT, "Could not connect").
-define(ERROR_COULD_NOT_RECOVER, "Could not recover").
-define(ERROR_NODE_NOT_EXISTS, "Node not exist").
-define(ERROR_TABLE_NOT_EXISTS, "Tables not exist").
-define(ERROR_FAILED_COMPACTION, "Failed compaction").
%% -define(ERROR_FAILED_GET_STORAGE_STATS, "Failed to get storage stats").
-define(ERROR_USER_NOT_FOUND, "User not found").
-define(ERROR_COULD_NOT_GET_USER, "Could not get user(s)").
-define(ERROR_COULD_NOT_ADD_USER, "Could not add a user").
-define(ERROR_COULD_NOT_REMOVE_USER, "Could not delete a user").
-define(ERROR_COULD_NOT_UPDATE_USER, "Could not update a user").
-define(ERROR_ENDPOINT_NOT_FOUND, "Endpoint not found").
-define(ERROR_COULD_NOT_SET_ENDPOINT, "Could not set an endpoint").
-define(ERROR_COULD_NOT_GET_ENDPOINT, "Could not get endpoint(s)").
-define(ERROR_COULD_NOT_REMOVE_ENDPOINT, "Could not remove an endpoint").
-define(ERROR_COULD_NOT_ATTACH_NODE, "Could not attach the node").
-define(ERROR_COULD_NOT_DETACH_NODE, "Could not detach the node").
-define(ERROR_COULD_NOT_SUSPEND_NODE, "Could not suspend the node").
-define(ERROR_COULD_NOT_RESUME_NODE, "Could not resume the node").
-define(ERROR_COULD_NOT_ROLLBACK, "Could not rollback the node").
-define(ERROR_COULD_NOT_UPDATE_NODE, "Could not update state of a node").
-define(ERROR_COULD_NOT_UPDATE_MANAGER, "Could not update manager(s)").
-define(ERROR_COULD_NOT_UPDATE_CONF, "Could not update the system conf").
-define(ERROR_COULD_NOT_CREATE_RING, "Could not create RING").
-define(ERROR_COULD_NOT_GET_REC, "Could not get records").
-define(ERROR_MNESIA_PROC_FAILURE, "Mnesia: processing failure").
-define(ERROR_MNESIA_WAIT_FOR_TABLE_TIMEOUT, "Mnesia: timeout of waiting for table").
-define(ERROR_MNESIA_WAIT_FOR_TABLE_ERROR, "Mnesia: failure of waiting for table").
-define(ERROR_MNESIA_GET_TABLE_INFO_ERROR, "Mnesia: failure of retrieving table-info").
-define(ERROR_NOT_SPECIFIED_COMMAND, "Command not exist").
-define(ERROR_NOT_SPECIFIED_NODE, "Not specified node").
-define(ERROR_NO_CMODE_SPECIFIED, "Not specified compaction mode").
-define(ERROR_INVALID_PATH, "Invalid path").
-define(ERROR_INVALID_ARGS, "Invalid arguments").
-define(ERROR_COULD_NOT_STORE, "Could not store value").
-define(ERROR_INVALID_BUCKET_FORMAT, "Invalid bucket format").
-define(ERROR_BUCKET_NOT_FOUND, "Bucket not found").
-define(ERROR_DEL_BUCKET_STATS_NOT_FOUND, "Delete-bucket's stats not found").
-define(ERROR_COULD_NOT_GET_BUCKET, "Could not get bucket(s)").
-define(ERROR_COULD_NOT_UPDATE_BUCKET, "Could not update bucket(s)").
-define(ERROR_SAME_BUCKET_EXISTS, "Same bucket exists").
-define(ERROR_NOT_STARTED, "Storage cluster is not running, yet").
-define(ERROR_ALREADY_STARTED, "Storage cluster already started").
-define(ERROR_STILL_RUNNING, "still running").
-define(ERROR_MNESIA_NOT_START, "Mnesia does not start, yet").
-define(ERROR_NOT_SATISFY_CONDITION, "Not satisfy conditions").
-define(ERROR_TARGET_NODE_NOT_RUNNING, "Target node does not running").
%% -define(ERROR_FAILED_BACKUP_MNESIA, "Failed to backup the mnesia backup file").
%% -define(ERROR_FAILED_RESTORE_MNESIA, "Failed to restore the mnesia backup file").
%% -define(ERROR_FAILED_UPDATE_MANAGERS, "Failed to update the manager nodes").
-define(ERROR_COULD_NOT_GET_CONF, "Could not get the system-config").
-define(ERROR_MEMBER_NOT_FOUND, "Member not found").
-define(ERROR_COULD_NOT_GET_MEMBER, "Could not get members (storage-nodes)").
-define(ERROR_COULD_NOT_GET_GATEWAY, "Could not get gateway(s)").
-define(ERROR_COULD_NOT_ACCESS_GATEWAY, "Could not access gateway(s)").
-define(ERROR_NOT_NEED_REBALANCE, "Not need rebalance").
-define(ERROR_FAIL_REBALANCE, "Fail rebalance").
-define(ERROR_FAIL_TO_ASSIGN_NODE, "Fail to assign node(s)").
-define(ERROR_FAIL_TO_REMOVE_NODE, "Fail to remove a node").
-define(ERROR_FAIL_TO_SYNCHRONIZE_RING, "Fail to synchronize RING").
-define(ERROR_FAIL_TO_UPDATE_ACL, "Fail to update acl of a bucket").
-define(ERROR_FAIL_ACCESS_MNESIA, "Fail to access mnesia").
-define(ERROR_ALREADY_HAS_SAME_CLUSTER, "Already has a same neme of cluster").
-define(ERROR_INCOMPATIBLE_VERSION, "Incompatible Version").
-define(ERROR_COULD_NOT_GET_CLUSTER_INFO,"Could not get cluster info").
-define(ERROR_OVER_MAX_CLUSTERS, "Over max number of clusters").
-define(ERROR_CLUSTER_NOT_FOUND, "Cluster not found").
-define(ERROR_NOT_REMOTE_CLUSTER, "Not remote cluster").
-define(ERROR_UPDATED_SYSTEM_CONF, "Updated the system configuration").
-define(ERROR_FAILED_UPDATE_LOG_LEVEL, "Failed to update the log-level").
-define(ERROR_FAILED_GET_VERSION, "Failed to get the version").
-define(ERROR_FAILED_REGISTERING_DEL_BUCKET_MSG, "Failed to register a del-bucket's message").
-define(ERROR_FAILED_REMOVING_DEL_BUCKET_MSG, "Failed to remove a del-bucket's message").
-define(ERROR_FAILED_RECOVER_DISK_DUE_TO_DIFFERENT_AVS_CONF, "Failed to recover disk due to different AVS conf found").

%% type of console
-define(CONSOLE_CUI, 'cui').
-define(CONSOLE_JSON, 'json').
-define(MOD_TEXT_FORMATTER, 'leo_manager_formatter_text').
-define(MOD_JSON_FORMATTER, 'leo_manager_formatter_json').

%% test values and default values
-define(TEST_USER_ID, <<"_test_leofs">>).
-define(TEST_ACCESS_KEY, <<"05236">>).
-define(TEST_SECRET_KEY, <<"802562235">>).

-define(DEF_ENDPOINT_1, <<"localhost">>).
-define(DEF_ENDPOINT_2, <<"s3.amazonaws.com">>).

-define(PROP_MNESIA_NODES, 'leo_manager_mnesia_nodes').
-define(DEF_MNESIA_DIR, "./work/mnesia/127.0.0.1").
-define(DEF_QUEUE_DIR, "./work/queue/").
-define(DEF_LOG_DIR, "./log/").

%% Command history related
-define(LOG_GROUP_ID_HISTORY, 'log_grp_history_log').
-define(LOG_ID_HISTORY, 'log_id_history_log').
-define(LOG_FILENAME_HISTORY, "cmd_history").
-define(put_cmd_history(_CmdBody),
        begin
            leo_logger_api:append(
              {?LOG_ID_HISTORY,
               #message_log{format  = "~s\t~w\t~s",
                            message = [leo_date:date_format(),
                                       leo_date:clock(),
                                       binary_to_list(_CmdBody)
                                      ]}
              })
        end).



%% MQ related:
-define(QUEUE_ID_FAIL_REBALANCE, 'mq_fail_rebalance').
-define(QUEUE_ID_REQ_DEL_BUCKET, 'mq_req_del_bucket').


-define(dequeue_interval(),
        begin
            _DEQUEUE_INTERVAL_MIN = ?env_dequeue_interval_min(),
            _DEQUEUE_INTERVAL_MAX = ?env_dequeue_interval_max(),

            case erlang:phash2(
                   term_to_binary({node(), leo_date:clock()}), _DEQUEUE_INTERVAL_MAX) of
                _Interval when _Interval < _DEQUEUE_INTERVAL_MIN ->
                    _DEQUEUE_INTERVAL_MIN;
                _Interval ->
                    _Interval
            end
        end).


%% ---------------------------------------------------------
%% RECORDS
%% ---------------------------------------------------------
-define(AUTH_NOT_YET, 0).
-define(AUTH_USERID_1, 1).
-define(AUTH_USERID_2, 2).
-define(AUTH_PASSWORD, 3).
-define(AUTH_DONE, 5).
-type(auth() :: ?AUTH_NOT_YET  |
                ?AUTH_USERID_1 |
                ?AUTH_USERID_2 |
                ?AUTH_PASSWORD |
                ?AUTH_DONE).

-ifdef(TEST).
-record(state,
        {formatter :: atom(),
         auth = ?AUTH_DONE :: auth(),
         user_id = <<>> :: binary(),
         password = <<>> :: binary(),
         plugin_mod :: atom()
        }).
-else.
-record(state,
        {formatter :: atom(),
         auth = ?AUTH_DONE :: auth(),
         user_id = <<>> :: binary(),
         password = <<>> :: binary(),
         plugin_mod :: atom()
        }).
-endif.

-record(rebalance_info,
        {vnode_id = -1  :: integer(),
         node :: atom(),
         total_of_objects = 0 :: integer(),
         num_of_remains = 0 :: integer(),
         when_is = 0 :: integer() %% Posted at
        }).

-record(history,
        {id = 1 :: pos_integer(),
         command = [] :: string(), %% Command
         created = -1 :: integer() %% Created At
        }).

-record(recovery_rebalance_info,
        {id = 1 :: pos_integer(),
         node :: atom(),
         rebalance_info = [] :: list(tuple()),
         timestamp = 1 :: pos_integer()
        }).

-record(node_state_for_output,
        {node = [] :: string(),
         type = [] :: string(),
         state :: atom(),
         rack_id = [] :: string(),
         ring_hash_new = [] :: string(),
         ring_hash_old = [] :: string(),
         when_is = 0 :: non_neg_integer()
        }).


-define(STATE_INVALID, 0).
-define(STATE_PENDING, 1).
-define(STATE_ENQUEUING, 2).
-define(STATE_MONITORING, 3).
-define(STATE_FINISHED, 9).
-type(del_bucket_state() :: ?STATE_PENDING |
                            ?STATE_ENQUEUING |
                            ?STATE_MONITORING |
                            ?STATE_FINISHED).
-record(del_bucket_queue,
        {bucket_name = <<>> :: binary(),
         access_key_bin = <<>> :: binary(),
         state = ?STATE_PENDING :: del_bucket_state(),
         timestamp = 1 :: pos_integer()
        }).

-record(del_bucket_state,
        {id = <<>> :: binary(),
         bucket_name = <<>> :: binary(),
         node :: atom(),
         state = ?STATE_PENDING :: del_bucket_state(),
         timestamp = 1 :: pos_integer()
        }).

-define(NODE_TYPE_GATEWAY, 0).
-define(NODE_TYPE_STORAGE, 1).
-type(dest_node_type() :: ?NODE_TYPE_GATEWAY |
                          ?NODE_TYPE_STORAGE).
-record(del_bucket_request,
        {id = 1 :: pos_integer(),
         node_type = ?NODE_TYPE_GATEWAY :: dest_node_type(),
         bucket_name = <<>> :: binary(),
         node :: node(),
         timestamp = 1 :: pos_integer()
        }).


-define(del_bucket_state(_State),
        begin
            case _State of
                'pending' ->
                    ?STATE_PENDING;
                'enqueuing' ->
                    ?STATE_ENQUEUING;
                'monitoring' ->
                    ?STATE_MONITORING;
                'finished' ->
                    ?STATE_FINISHED
            end
        end).

-define(del_bucket_state_str(_State),
        begin
            case _State of
                ?STATE_PENDING ->
                    "pending";
                ?STATE_ENQUEUING ->
                    "enqueuing";
                ?STATE_MONITORING ->
                    "monitoring";
                ?STATE_FINISHED ->
                    "finished"
            end
        end).



%% ---------------------------------------------------------
%% MACROS
%% ---------------------------------------------------------
-define(MANAGER_TYPE_MASTER, 'master').
-define(MANAGER_TYPE_SLAVE, 'slave').
-type(manager_type() :: ?MANAGER_TYPE_MASTER |
                        ?MANAGER_TYPE_SLAVE).

-define(env_mode_of_manager(),
        case application:get_env(leo_manager, manager_mode) of
            {ok, EnvModeOfManager} -> EnvModeOfManager;
            _ -> ?MANAGER_TYPE_MASTER
        end).

-define(env_partner_of_manager_node(),
        case application:get_env(leo_manager, manager_partners) of
            {ok, EnvPartnerOfManagerNode} -> EnvPartnerOfManagerNode;
            _ -> []
        end).

-define(env_bind_address(),
        case application:get_env(leo_manager, bind_address) of
            {ok, EnvBindAddress} -> EnvBindAddress;
            _ -> "localhost"
        end).

-define(env_listening_port_cui(),
        case application:get_env(leo_manager, port_cui) of
            {ok, EnvCUIListeningPort} -> EnvCUIListeningPort;
            _ -> 10010
        end).

-define(env_listening_port_json(),
        case application:get_env(leo_manager, port_json) of
            {ok, EnvJSONListeningPort} -> EnvJSONListeningPort;
            _ -> 10020
        end).

-define(env_num_of_acceptors_cui(),
        case application:get_env(leo_manager, num_of_acceptors_cui) of
            {ok, EnvCUINumOfAcceptors} -> EnvCUINumOfAcceptors;
            _ -> 3
        end).

-define(env_num_of_acceptors_json(),
        case application:get_env(leo_manager, num_of_acceptors_json) of
            {ok, EnvJSONNumOfAcceptors} -> EnvJSONNumOfAcceptors;
            _ -> 3
        end).

-define(env_console_num_of_histories(),
        case application:get_env(leo_manager, num_of_histories) of
            {ok, EnvNumOfHistories} -> EnvNumOfHistories;
            _ -> 200
        end).

-define(env_console_user_id(),
        case application:get_env(leo_manager, console_user_id) of
            {ok, EnvConsoleUserId} -> EnvConsoleUserId;
            _ -> "leo"
        end).

-define(env_console_password(),
        case application:get_env(leo_manager, console_password) of
            {ok, EnvConsolePassword} -> EnvConsolePassword;
            _ -> "faststorage"
        end).

-define(env_num_of_compact_proc(),
        case application:get_env(leo_manager, num_of_compact_proc) of
            {ok, EnvConsoleNumOfCompactProc} -> EnvConsoleNumOfCompactProc;
            _ -> 3
        end).

-define(env_available_commands(),
        case application:get_env(leo_manager, available_commands) of
            {ok, EnvAvailableCommands} -> EnvAvailableCommands;
            _ -> all
        end).

-define(ratio_of_active_size(_ActiveSize, _TotalSize),
        case (TotalSize < 1) of
            true  -> 0;
            false ->
                erlang:round((_ActiveSize / _TotalSize) * 10000)/100
        end).

-define(env_use_s3_api(),
        %% default is true
        case application:get_env(leo_manager, use_s3_api) of
            {ok, EnvUseS3API} -> EnvUseS3API;
            _ -> true
        end).

-define(env_log_dir(),
        case application:get_env(leo_manager, log_appender) of
            {ok, [{file, Options}|_]} ->
                leo_misc:get_value(path, Options, ?DEF_LOG_DIR);
            _ ->
                ?DEF_LOG_DIR
        end).

-define(env_queue_dir(),
        case application:get_env(leo_manager, queue_dir) of
            {ok, _EnvQueueDir} ->
                _EnvQueueDir;
            _ ->
                ?DEF_QUEUE_DIR
        end).


-define(env_dequeue_interval_min(),
        case application:get_env(leo_manager, dequeue_interval_min) of
            {ok, EnvDequeueIntervalMin} -> EnvDequeueIntervalMin;
            _ -> timer:seconds(10)
        end).

-define(env_dequeue_interval_max(),
        case application:get_env(leo_manager, dequeue_interval_max) of
            {ok, EnvDequeueIntervalMax} -> EnvDequeueIntervalMax;
            _ -> timer:seconds(30)
        end).



%% @doc Plugin-related macros
%%
-define(env_plugin_mod_console(),
        case application:get_env(leo_manager, plugin_mod_console) of
            {ok, EnvPluginModConsole} -> EnvPluginModConsole;
            _ -> undefined
        end).

-define(env_plugin_mod_mnesia(),
        case application:get_env(leo_manager, plugin_mod_mnesia) of
            {ok, EnvPluginModMnesia} -> EnvPluginModMnesia;
            _ -> undefined
        end).


%% @doc boolean value to enabled/disabled
%%
-define(BOOL_TO_ENABLE, [{true,  enabled},
                         {false, disabled}]).

%% @doc Retrieve tokens
%%
-define(get_tokens(_Option,_ErrMsg),
        begin
            case string:tokens(binary_to_list(_Option), ?COMMAND_DELIMITER) of
                [] ->
                    {error,_ErrMsg};
                _Tokens ->
                    {ok, _Tokens}
            end
        end).
