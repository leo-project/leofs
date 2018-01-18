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
%% software distributed under the License is distribute0d on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%%======================================================================
-module(leo_manager_console).

-include("leo_manager.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_auth.hrl").
-include_lib("leo_s3_libs/include/leo_s3_user.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/2, start_link/3, stop/0]).
-export([init/1, handle_call/3]).


%%----------------------------------------------------------------------
%% API
%%----------------------------------------------------------------------
start_link(Formatter, Params) ->
    start_link(Formatter, Params, undefined).

start_link(Formatter, Params, PluginMod) ->
    tcp_server:start_link(?MODULE, [Formatter, PluginMod], Params).

stop() ->
    tcp_server:stop().


%%----------------------------------------------------------------------
%% Callback function(s)
%%----------------------------------------------------------------------
init([Formatter, PluginMod]) ->
    {ok, #state{formatter  = Formatter,
                plugin_mod = PluginMod
               }}.


%%----------------------------------------------------------------------
%% Operation-1
%%----------------------------------------------------------------------
%% Command: "help"
handle_call(_Socket, <<?CMD_HELP, ?CRLF>>, #state{formatter  = Formatter,
                                                  plugin_mod = PluginMod} = State) ->
    Fun = fun() ->
                  Formatter:help(PluginMod)
          end,
    Reply = invoke(?CMD_HELP, Formatter, Fun),
    {reply, Reply, State};


%% Command: "version"
handle_call(_Socket, <<?CMD_VERSION, ?LF>>, #state{formatter = Formatter} = State) ->
    Reply = version(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_VERSION, ?CRLF>>, #state{formatter = Formatter} = State) ->
    Reply = version(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_VERSION, ?SPACE, Option/binary>>, #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case version_all(Option) of
                      {ok, NodeList} ->
                          Formatter:version_all(NodeList);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_VERSION, Formatter, Fun),
    {reply, Reply, State};


%% Command: "_user-id_"
handle_call(_Socket, ?USER_ID, #state{formatter = Formatter} = State) ->
    Reply = Formatter:user_id(),
    {reply, Reply, State};


%% Command: "_password_"
handle_call(_Socket, ?PASSWORD, #state{formatter = Formatter} = State) ->
    Reply = Formatter:password(),
    {reply, Reply, State};


%% Command: "_authorized_"
handle_call(_Socket, ?AUTHORIZED, #state{formatter = Formatter} = State) ->
    Reply = Formatter:authorized(),
    {reply, Reply, State};


%% Command: "_authorized_"
handle_call(_Socket, <<?LOGIN, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Reply = case login(Option) of
                {ok, User, Credential} ->
                    Formatter:login(User, Credential);
                {error, Cause} ->
                    Formatter:error(Cause)
            end,
    {reply, Reply, State};


%% Command: "status"
%% Command: "status ${NODE_NAME}"
handle_call(_Socket, <<?CMD_STATUS, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case get_status(Option) of
                      {ok, {node_list, Props}} ->
                          Formatter:system_info_and_nodes_stat(Props);
                      {ok, {?SERVER_TYPE_GATEWAY = Type, NodeStatus}} ->
                          Formatter:node_stat(Type, NodeStatus);
                      {ok, {?SERVER_TYPE_STORAGE = Type, NodeStatus}} ->
                          Formatter:node_stat(Type, NodeStatus);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_STATUS, Formatter, Fun),
    {reply, Reply, State};


%% Command : "detach ${NODE_NAME}"
handle_call(_Socket, <<?CMD_DETACH, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case detach(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DETACH, Formatter, Fun),
    {reply, Reply, State};


%% Command: "suspend ${NODE_NAME}"
handle_call(_Socket, <<?CMD_SUSPEND, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case suspend(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_SUSPEND, Formatter, Fun),
    {reply, Reply, State};


%% Command: "resume ${NODE_NAME}"
handle_call(_Socket, <<?CMD_RESUME, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case resume(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_RESUME, Formatter, Fun),
    {reply, Reply, State};


%% Command: "rollback ${NODE_NAME}"
handle_call(_Socket, <<?CMD_ROLLBACK, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case rollback(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_ROLLBACK, Formatter, Fun),
    {reply, Reply, State};


%% Command: "start"
handle_call(Socket, <<?CMD_START, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = start(Socket, Formatter),
    {reply, Reply, State};
handle_call(Socket, <<?CMD_START, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = start(Socket, Formatter),
    {reply, Reply, State};


%% Command: "rebalance"
handle_call(Socket, <<?CMD_REBALANCE, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = rebalance(Socket, Formatter),
    {reply, Reply, State};
handle_call(Socket, <<?CMD_REBALANCE, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = rebalance(Socket, Formatter),
    {reply, Reply, State};


%% Command: "update-property" for watchdog
handle_call(_Socket, <<?CMD_UPDATE_PROP, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_property(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_PROP, Formatter, Fun),
    {reply, Reply, State};

%%----------------------------------------------------------------------
%% Operation-2
%%----------------------------------------------------------------------
%% Command: "du ${NODE_NAME}"
handle_call(_Socket, <<?CMD_DU, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case du(Option) of
                      {ok, {Option1, StorageStats}} ->
                          Formatter:du(Option1, StorageStats);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DU, Formatter, Fun),
    {reply, Reply, State};


%% Command: "compact ${NODE_NAME}"
handle_call(_Socket, <<?CMD_COMPACT, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case compact(Option) of
                      ok ->
                          Formatter:ok();
                      {ok, Status} ->
                          Formatter:compact_status(Status);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_COMPACT, Formatter, Fun),
    {reply, Reply, State};

%% Command: "diagnose-data ${NODE_NAME}"
handle_call(_Socket, <<?CMD_DIAGNOSE_DATA, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case diagnose_data(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DIAGNOSE_DATA, Formatter, Fun),
    {reply, Reply, State};


%%----------------------------------------------------------------------
%% Operation for MQ
%%----------------------------------------------------------------------
handle_call(_Socket, <<?CMD_MQ_STATS, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case mq_stats(Option) of
                      {ok, Stats} ->
                          Formatter:mq_stats(Stats);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_MQ_STATS, Formatter, Fun),
    {reply, Reply, State};

handle_call(_Socket, <<?CMD_MQ_SUSPEND, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case mq_suspend(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_MQ_SUSPEND, Formatter, Fun),
    {reply, Reply, State};

handle_call(_Socket, <<?CMD_MQ_RESUME, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case mq_resume(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_MQ_RESUME, Formatter, Fun),
    {reply, Reply, State};


%%----------------------------------------------------------------------
%% Operation-3
%%----------------------------------------------------------------------
%% Command: "create-user ${USER_ID} ${PASSWORD}"
handle_call(_Socket, <<?CMD_CREATE_USER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case create_user(Option) of
                      {ok, PropList} ->
                          AccessKeyId     = leo_misc:get_value('access_key_id',     PropList),
                          SecretAccessKey = leo_misc:get_value('secret_access_key', PropList),
                          Formatter:credential(AccessKeyId, SecretAccessKey);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_CREATE_USER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "import-user ${USER_ID} ${access-key-id} ${secret-access-key}"
handle_call(_Socket, <<?CMD_IMPORT_USER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case import_user(Option) of
                      {ok, PropList} ->
                          AccessKeyId     = leo_misc:get_value('access_key_id',     PropList),
                          SecretAccessKey = leo_misc:get_value('secret_access_key', PropList),
                          Formatter:credential(AccessKeyId, SecretAccessKey);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_IMPORT_USER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "update-user-role ${USER_ID} ${ROLE}"
handle_call(_Socket, <<?CMD_UPDATE_USER_ROLE, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_user_role(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_USER_ROLE, Formatter, Fun),
    {reply, Reply, State};


%% Command: "update-user-password ${USER_ID} ${PASSWORD}"
handle_call(_Socket, <<?CMD_UPDATE_USER_PW, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_user_password(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_USER_PW, Formatter, Fun),
    {reply, Reply, State};


%% Command: "delete-user ${USER_ID} ${PASSWORD}"
handle_call(_Socket, <<?CMD_DELETE_USER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case delete_user(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DELETE_USER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "get-users"
handle_call(_Socket, <<?CMD_GET_USERS, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_users(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_GET_USERS, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_users(Formatter),
    {reply, Reply, State};


%% Command: "end-endpoint ${END_POINT}"
handle_call(_Socket, <<?CMD_ADD_ENDPOINT, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case set_endpoint(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_ADD_ENDPOINT, Formatter, Fun),
    {reply, Reply, State};

handle_call(_Socket, <<?CMD_SET_ENDPOINT, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case set_endpoint(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_SET_ENDPOINT, Formatter, Fun),
    {reply, Reply, State};


%% Command: "get-endpoints"
handle_call(_Socket, <<?CMD_GET_ENDPOINTS, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_endpoints(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_GET_ENDPOINTS, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_endpoints(Formatter),
    {reply, Reply, State};


%% Command: "del-endpoint ${end_point}"
handle_call(_Socket, <<?CMD_DEL_ENDPOINT, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case delete_endpoint(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DEL_ENDPOINT, Formatter, Fun),
    {reply, Reply, State};


%% Command: "add-buckets ${bucket} ${access-key-id}"
handle_call(_Socket, <<?CMD_ADD_BUCKET, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case add_bucket(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_ADD_BUCKET, Formatter, Fun),
    {reply, Reply, State};


%% Command: "delete-bucket ${bucket} ${access-key-id}"
handle_call(_Socket, <<?CMD_DELETE_BUCKET, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case delete_bucket(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DELETE_BUCKET, Formatter, Fun),
    {reply, Reply, State};

%% Command: "delete-bucket-stats ${bucket}"
handle_call(Socket, <<?CMD_DELETE_BUCKET_STATS, ?LF>>, State) ->
    handle_call(Socket, <<?CMD_DELETE_BUCKET_STATS, ?CRLF>>, State);
handle_call(_Socket, <<?CMD_DELETE_BUCKET_STATS, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case delete_bucket_stats_all() of
                      {ok, Stats} ->
                          Formatter:del_bucket_stats_all(Stats);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DELETE_BUCKET_STATS, Formatter, Fun),
    {reply, Reply, State};

handle_call(_Socket, <<?CMD_DELETE_BUCKET_STATS, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case delete_bucket_stats(Option) of
                      {ok, Stats} ->
                          Formatter:del_bucket_stats(Stats);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DELETE_BUCKET_STATS, Formatter, Fun),
    {reply, Reply, State};

%% Command: "reset-delete-bucket-stats ${bucket}"
handle_call(_Socket, <<?CMD_RESET_DELETE_BUCKET_STATS, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case reset_delete_bucket_stats(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_RESET_DELETE_BUCKET_STATS, Formatter, Fun),
    {reply, Reply, State};

%% Command: "get-buckets"
handle_call(_Socket, <<?CMD_GET_BUCKETS, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_buckets(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_GET_BUCKETS, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = get_buckets(Formatter),
    {reply, Reply, State};


%% Command: "get-bucket ${access-key-id}"
handle_call(_Socket, <<?CMD_GET_BUCKET_BY_ACCESS_KEY, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case get_bucket_by_access_key(Option) of
                      {ok, Buckets} ->
                          Formatter:bucket_by_access_key(Buckets);
                      not_found ->
                          Formatter:error("Bucket not found");
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_GET_BUCKETS, Formatter, Fun),
    {reply, Reply, State};


%% Command: "chown-bucket ${bucket} ${new-access-key-id}"
handle_call(_Socket, <<?CMD_CHANGE_BUCKET_OWNER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case change_bucket_owner(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_CHANGE_BUCKET_OWNER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "set-redundancy-method ${bucket} ${access-key-id} ${redundancy-method}"
%%
handle_call(_Socket, <<?CMD_SET_RED_METHOD, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case set_redundancy_method(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_SET_RED_METHOD, Formatter, Fun),
    {reply, Reply, State};


%% Command: "update-acl ${bucket} ${canned_acl}"
handle_call(_Socket, <<?CMD_UPDATE_ACL, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_acl(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_ACL, Formatter, Fun),
    {reply, Reply, State};


%% Command: "whereis ${PATH}"
handle_call(_Socket, <<?CMD_WHEREIS, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case get_assignments(Option) of
                      {ok, AssignedInfo} ->
                          Formatter:whereis(AssignedInfo);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_WHEREIS, Formatter, Fun),
    {reply, Reply, State};


%% Command: "recover file|node ${PATH}|${NODE}"
handle_call(Socket, <<?CMD_RECOVER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Socket_1 = case Formatter of
                   ?MOD_TEXT_FORMATTER ->
                       Socket;
                   _ ->
                       null
               end,
    Fun = fun() ->
                  case recover(Socket_1, Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_RECOVER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "purge ${PATH}"
handle_call(_Socket, <<?CMD_PURGE, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case purge(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_PURGE, Formatter, Fun),
    {reply, Reply, State};


%% Command: "remove ${GATEWAY_NODE}"
handle_call(_Socket, <<?CMD_REMOVE, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case remove(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_REMOVE, Formatter, Fun),
    {reply, Reply, State};


%% Command: "backup-mnesia ${MNESIA_BACKUPFILE}"
handle_call(_Socket, <<?CMD_BACKUP_MNESIA, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case backup_mnesia(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_BACKUP_MNESIA, Formatter, Fun),
    {reply, Reply, State};


%% Command: "restore-mnesia ${MNESIA_BACKUPFILE}"
handle_call(_Socket, <<?CMD_RESTORE_MNESIA, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case restore_mnesia(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_RESTORE_MNESIA, Formatter, Fun),
    {reply, Reply, State};


%% Command: "update-managers ${MANAGER_MASTER} ${MANAGER_SLAVE}"
handle_call(_Socket, <<?CMD_UPDATE_MANAGERS, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_manager_nodes(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_MANAGERS, Formatter, Fun),
    {reply, Reply, State};


%% @deplicated
%% %% Command: "history"
%% handle_call(_Socket, <<?CMD_HISTORY, ?CRLF>>, #state{formatter = Formatter} = State) ->
%%     Fun = fun() ->
%%                   case leo_manager_mnesia:get_histories() of
%%                       {ok, Histories} ->
%%                           Formatter:histories(Histories);
%%                       not_found ->
%%                           Formatter:histories([]);
%%                       {error, Cause} ->
%%                           Formatter:error(Cause)
%%                   end
%%           end,
%%     Reply = invoke(?CMD_HISTORY, Formatter, Fun),
%%     {reply, Reply, State};


%% Command: "dump-ring ${NODE}"
handle_call(_Socket, <<?CMD_DUMP_RING, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case dump_ring(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_DUMP_RING, Formatter, Fun),
    {reply, Reply, State};


%% Command: "update-log-level ${NODE} ${LOG_LEVEL}"
handle_call(_Socket, <<?CMD_UPDATE_LOG_LEVEL, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_log_level(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_LOG_LEVEL, Formatter, Fun),
    {reply, Reply, State};

%% Command: "update-consistency-level ${NODE} ${WRITE_QUORUM} ${READ_QUORUM} ${DELETE_QUORUM}"
handle_call(_Socket, <<?CMD_UPDATE_CONSISTENCY_LEVEL, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case update_consistency_level(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_UPDATE_LOG_LEVEL, Formatter, Fun),
    {reply, Reply, State};

%% Command: "gen-nfs-mnt-key ${BUCKET} ${ACCESS-KEY-ID} ${IP-ADDRESS}"
handle_call(_Socket, <<?CMD_GEN_NFS_MNT_KEY, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case gen_nfs_mnt_key(Option) of
                      {ok, Key} ->
                          Formatter:nfs_mnt_key(Key);
                      {error,_Cause} ->
                          Formatter:error(?ERROR_INVALID_ARGS)
                  end
          end,
    Reply = invoke(?CMD_GEN_NFS_MNT_KEY, Formatter, Fun),
    {reply, Reply, State};


%%----------------------------------------------------------------------
%% Operation-4
%%----------------------------------------------------------------------
%% Command: "join-cluster [${REMOTE_MANAGER_NODE}, ${REMOTE_MANAGER_NODE}, ...]"
handle_call(_Socket, <<?CMD_JOIN_CLUSTER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case join_cluster(Option) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_JOIN_CLUSTER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "remove-cluster [${REMOTE_MANAGER_NODE}, ${REMOTE_MANAGER_NODE}, ...]"
handle_call(_Socket, <<?CMD_REMOVE_CLUSTER, ?SPACE, Option/binary>>,
            #state{formatter = Formatter} = State) ->
    Fun = fun() ->
                  case remove_cluster(Option) of
                      ok ->
                          Formatter:ok();
                      not_found ->
                          Formatter:error(?ERROR_CLUSTER_NOT_FOUND);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    Reply = invoke(?CMD_REMOVE_CLUSTER, Formatter, Fun),
    {reply, Reply, State};


%% Command: "cluster-status"
handle_call(_Socket, <<?CMD_CLUSTER_STAT, ?LF>>,
            #state{formatter = Formatter} = State) ->
    Reply = cluster_status(Formatter),
    {reply, Reply, State};
handle_call(_Socket, <<?CMD_CLUSTER_STAT, ?CRLF>>,
            #state{formatter = Formatter} = State) ->
    Reply = cluster_status(Formatter),
    {reply, Reply, State};


%% Command: "quit"
handle_call(_Socket, <<?CMD_QUIT, ?CRLF>>, State) ->
    {close, <<?BYE>>, State};


handle_call(_Socket, <<?CRLF>>, State) ->
    {reply, "", State};


handle_call(_Socket, _Data, #state{formatter = Formatter,
                                   plugin_mod = undefined} = State) ->
    Reply = Formatter:error(?ERROR_NOT_SPECIFIED_COMMAND),
    {reply, Reply, State};

handle_call(Socket, Data, #state{plugin_mod = PluginMod} = State) ->
    PluginMod:handle_call(Socket, Data, State).


%%----------------------------------------------------------------------
%% Inner function(s)
%%----------------------------------------------------------------------
%% Invoke a command
%% @private
-spec(invoke(string(), atom(), function()) ->
             binary()).
invoke(Command, Formatter, Fun) ->
    case leo_manager_mnesia:get_available_command_by_name(Command) of
        not_found ->
            Formatter:error(?ERROR_NOT_SPECIFIED_COMMAND);
        _ ->
            Fun()
    end.


%% @doc Retrieve the version
%% @private
version(Formatter) ->
    {ok, Version} = version(),
    Formatter:version(Version).


%% @doc Launch the LeoFS
%% @private
start(Socket, Formatter) ->
    Socket_1 = case Formatter of
                   ?MOD_TEXT_FORMATTER -> Socket;
                   _ -> null
               end,

    Fun = fun() ->
                  case start(Socket_1) of
                      ok ->
                          Formatter:ok();
                      {error, {bad_nodes, BadNodes}} ->
                          Formatter:bad_nodes(BadNodes);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_START, Formatter, Fun).


%% @doc Execute the rebalance
%% @private
rebalance(Socket, Formatter) ->
    Socket_1 = case Formatter of
                   ?MOD_TEXT_FORMATTER -> Socket;
                   _ -> null
               end,

    Fun = fun() ->
                  case leo_manager_api:rebalance(Socket_1) of
                      ok ->
                          Formatter:ok();
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_REBALANCE, Formatter, Fun).


%% @doc Update a watchdog property
%% <p>
%% $ leofs-adm update-property watchdog.cpu_enabled <boolean>
%% $ leofs-adm update-property watchdog.cpu_raised_error_times <integer>
%% $ leofs-adm update-property watchdog.cpu_interval <integer>
%% $ leofs-adm update-property watchdog.cpu_threshold_load_avg <float>
%% $ leofs-adm update-property watchdog.cpu_threshold_util <integer>
%% $ leofs-adm update-property watchdog.disk_enabled <boolean>
%% $ leofs-adm update-property watchdog.disk_raised_error_times <integer>
%% $ leofs-adm update-property watchdog.disk_interval <integer>
%% $ leofs-adm update-property watchdog.disk_threshold_use <integer>
%% $ leofs-adm update-property watchdog.disk_threshold_util <integer>
%% $ leofs-adm update-property watchdog.disk_threshold_rkb <integer>
%% $ leofs-adm update-property watchdog.disk_threshold_wkb <integer>
%% $ leofs-adm update-property watchdog.cluster_enabled <boolean>
%% $ leofs-adm update-property watchdog.cluster_interval <integer>
%% </p>
%% @private
update_property(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_COMMAND) of
        {ok, [Node, PropertyName, PropertyValue|_]} ->
            update_property_1(Node, PropertyName, PropertyValue);
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @private
update_property_1(Node, "watchdog.cpu_enabled","true") ->
    update_property_2(Node, start, [cpu]);
update_property_1(Node, "watchdog.cpu_enabled","false") ->
    update_property_2(Node, stop, [cpu]);
update_property_1(Node, "watchdog.cpu_raised_error_times", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_raised_error_times, [cpu, Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.cpu_interval", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_check_interval, [cpu, Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.cpu_threshold_load_avg", Val) ->
    case exchange_datatype(float, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_cpu_threshold_load_avg, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.cpu_threshold_util", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_cpu_threshold_util, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_enabled","true") ->
    update_property_2(Node, start, [disk]);
update_property_1(Node, "watchdog.disk_enabled","false") ->
    update_property_2(Node, stop, [disk]);
update_property_1(Node, "watchdog.disk_raised_error_times", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_raised_error_times, [disk, Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_interval", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_check_interval, [disk, Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_threshold_use", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_disk_threshold_use, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_threshold_util", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_disk_threshold_util, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_threshold_rkb", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_disk_threshold_rkb, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.disk_threshold_wkb", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_disk_threshold_wkb, [Val_1]);
        Error ->
            Error
    end;
update_property_1(Node, "watchdog.cluster_enabled","true") ->
    update_property_2(Node, start, [cluster]);
update_property_1(Node, "watchdog.cluster_enabled","false") ->
    update_property_2(Node, stop, [cluster]);
update_property_1(Node, "watchdog.cluster_interval", Val) ->
    case exchange_datatype(integer, Val) of
        {ok, Val_1} ->
            update_property_2(Node, set_check_interval, [cluster, Val_1]);
        Error ->
            Error
    end;
update_property_1(_,_,_) ->
    {error, ?ERROR_NOT_SPECIFIED_COMMAND}.

%% @private
exchange_datatype(integer, Val) ->
    case catch list_to_integer(Val) of
        {'EXIT',_} ->
            {error, invalid_value};
        Val_1 ->
            {ok, Val_1}
    end;
exchange_datatype(float, Val) ->
    case catch list_to_float(Val) of
        {'EXIT',_} ->
            case exchange_datatype(integer, Val) of
                {ok, Val_1} ->
                    Val_1 * 1.0;
                Error ->
                    Error
            end;
        Val_1 ->
            {ok, Val_1}
    end.

%% @private
update_property_2(Node, Method, Args) when is_atom(Node) ->
    case leo_misc:node_existence(Node) of
        true ->
            case rpc:call(Node, leo_watchdog_api, Method, Args, ?TIMEOUT_FOR_LEOFSADM) of
                {badrpc,_} ->
                    {error, ?ERROR_COULD_NOT_CONNECT};
                Res ->
                    Res
            end;
        false ->
            {error, ?ERROR_NODE_NOT_EXISTS}
    end;
update_property_2(Node, Method, Args) ->
    update_property_2(list_to_atom(Node), Method, Args).


%% @doc Backup files of manager's mnesia
%% @private
-spec(backup_mnesia(binary()) ->
             ok | {error, any()}).
backup_mnesia(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [BackupFile|_]} ->
            leo_manager_mnesia:backup(BackupFile);
        Error ->
            Error
    end.


%% @doc Restore mnesia from backup files
%% @private
-spec(restore_mnesia(binary()) ->
             ok | {error, any()}).
restore_mnesia(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [BackupFile|_]} ->
            leo_manager_mnesia:restore(BackupFile);
        Error ->
            Error
    end.


%% @doc Update manager's node to alternate node
%% @private
-spec(update_manager_nodes(binary()) ->
             ok | {error, any()}).
update_manager_nodes(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Master, Slave|_]} ->
            leo_manager_api:update_manager_nodes(
              [list_to_atom(Master), list_to_atom(Slave)]);
        Error ->
            Error
    end.


%% @doc Output ring of a targe node
%% @private
dump_ring(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Node|_]} ->
            rpc:call(list_to_atom(Node),
                     leo_redundant_manager_api, dump, [both], ?TIMEOUT_FOR_LEOFSADM);
        Error ->
            Error
    end.


%% @doc Updaet a log level of a node
%% @private
update_log_level(Option) ->
    case string:tokens(binary_to_list(Option), ?COMMAND_DELIMITER) of
        [Node, LogLevel|_] ->
            {CanSendMsg, LogLevel_1} =
                case LogLevel of
                    "debug" ->
                        {ok, 0};
                    "info" ->
                        {ok, 1};
                    "warn" ->
                        {ok, 2};
                    "error" ->
                        {ok, 3};
                    _ ->
                        {{error, badarg}, -1}
                end,
            case CanSendMsg of
                ok ->
                    leo_manager_api:update_log_level(Node, LogLevel_1);
                Error ->
                    Error
            end;
        _ ->
            {error, badarg}
    end.


%% @doc Update a consistency level of a node
%% @private
update_consistency_level(Option) ->
    case string:tokens(binary_to_list(Option), ?COMMAND_DELIMITER) of
        [WStr, RStr, DStr|_] ->
            Ret = case catch list_to_integer(WStr) of
                      {'EXIT',_} ->
                          {error, badarg};
                      WInt ->
                          case catch list_to_integer(RStr) of
                              {'EXIT',_} ->
                                  {error, badarg};
                              RInt ->
                                  case catch list_to_integer(DStr) of
                                      {'EXIT',_} ->
                                          {error, badarg};
                                      DInt ->
                                          {ok, {WInt, RInt, DInt}}
                                  end
                          end
                  end,
            update_consistency_level_1(Ret);
        _ ->
            {error, badarg}
    end.

%% @private
update_consistency_level_1({error, Cause}) ->
    {error, Cause};
update_consistency_level_1({ok, {W, R, D} = ConsistencyLevel}) ->
    %% Validate the consistency level
    N = leo_redundant_manager_api:get_option(?PROP_N),
    case (N /= 0) of
        true when N >= W andalso W > 0 andalso
                  N >= R andalso R > 0 andalso
                  N >= D andalso D > 0  ->
            leo_manager_api:update_consistency_level(ConsistencyLevel);
        _ ->
            {error, ?ERROR_INVALID_ARGS}
    end.


%% @private
gen_nfs_mnt_key(Option) ->
    case string:tokens(binary_to_list(Option), ?COMMAND_DELIMITER) of
        [Bucket, AccessKey, IP|_] ->
            leo_s3_bucket:gen_nfs_mnt_key(list_to_binary(Bucket),
                                          list_to_binary(AccessKey),
                                          list_to_binary(IP));
        _ ->
            {error, ?ERROR_INVALID_ARGS}
    end.


%% @doc Join a cluster
%% @private
join_cluster(Option) ->
    case leo_cluster_tbl_conf:get() of
        {ok, #?SYSTEM_CONF{max_mdc_targets = MaxTargets}} ->
            case leo_mdcr_tbl_cluster_stat:all() of
                not_found ->
                    join_cluster_1(Option);
                {ok, Rows} when  MaxTargets < length(Rows) ->
                    join_cluster_1(Option);
                {ok, _}->
                    {error, ?ERROR_OVER_MAX_CLUSTERS};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_GET_CLUSTER_INFO}
            end;
        not_found ->
            {error, ?ERROR_COULD_NOT_GET_CONF};
        {error,_Cause} ->
            {error, ?ERROR_COULD_NOT_GET_CONF}
    end.

%% @private
-spec(join_cluster_1(binary()) ->
             {ok, atom()} | {error, any()}).
join_cluster_1(Bin) ->
    case ?get_tokens(Bin, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, Nodes} ->
            Nodes_1 = lists:map(fun(N) ->
                                        list_to_atom(N)
                                end, Nodes),
            case join_cluster_2(Nodes_1) of
                {ok, ClusterId} ->
                    leo_manager_api:update_cluster_manager(Nodes_1, ClusterId);
                Other ->
                    Other
            end;
        Error ->
            Error
    end.


%% @private
-spec(join_cluster_2([atom()]) ->
             {ok, atom()} | {error, any()}).
join_cluster_2([]) ->
    {error, ?ERROR_COULD_NOT_CONNECT};
join_cluster_2([Node|Rest] = RemoteNodes) ->
    case find_cluster_id_by_manager_node(Node) of
        {ok,_ClusterId} ->
            {error, ?ERROR_ALREADY_HAS_SAME_CLUSTER};
        not_found ->
            {ok, SystemConf} = leo_cluster_tbl_conf:get(),
            RPCNode  = leo_rpc:node(),
            Managers = case ?env_partner_of_manager_node() of
                           [] ->
                               [RPCNode];
                           [Partner|_] ->
                               case rpc:call(Partner, leo_rpc, node, [], ?TIMEOUT_FOR_LEOFSADM) of
                                   {_,_Cause} ->
                                       [RPCNode];
                                   Partner_1 ->
                                       [RPCNode, Partner_1]
                               end
                       end,

            case catch leo_rpc:call(Node, leo_manager_api, join_cluster,
                                    [Managers, SystemConf]) of
                {ok, #?SYSTEM_CONF{cluster_id = ClusterId} = RemoteSystemConf} ->
                    case leo_mdcr_tbl_cluster_info:get(ClusterId) of
                        not_found ->
                            #?SYSTEM_CONF{dc_id = DCId,
                                          n = N,
                                          r = R,
                                          w = W,
                                          d = D,
                                          bit_of_ring = BitOfRing,
                                          num_of_dc_replicas = NumOfReplicas,
                                          num_of_rack_replicas = NumOfRaclReplicas
                                         } = RemoteSystemConf,
                            case leo_mdcr_tbl_cluster_info:update(
                                   #?CLUSTER_INFO{cluster_id = ClusterId,
                                                  dc_id = DCId,
                                                  n = N, r = R, w = W, d = D,
                                                  bit_of_ring = BitOfRing,
                                                  num_of_dc_replicas = NumOfReplicas,
                                                  num_of_rack_replicas = NumOfRaclReplicas}) of
                                ok ->
                                    ok = leo_manager_api:sync_mdc_tables(
                                           ClusterId, RemoteNodes),
                                    {ok, ClusterId};
                                _Other ->
                                    {error, ?ERROR_FAIL_ACCESS_MNESIA}
                            end;
                        {ok, _} ->
                            {error, ?ERROR_ALREADY_HAS_SAME_CLUSTER};
                        _Other ->
                            {error, ?ERROR_FAIL_ACCESS_MNESIA}
                    end;
                _Error ->
                    join_cluster_2(Rest)
            end;
        {error, Cause} ->
            ?error("join_cluster_2/1", [{cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_CLUSTER_INFO}
    end.


%% @doc Find a clusterId by a remote manager-node
%% @private
find_cluster_id_by_manager_node(Node) ->
    case leo_mdcr_tbl_cluster_mgr:all() of
        {ok, ClusterMgrL} ->
            case lists:foldl(
                   fun(#cluster_manager{node = N,
                                        cluster_id = ClusterId},_SoFar) when N == Node ->
                           {ok, ClusterId};
                      (_, SoFar) ->
                           SoFar
                   end, undefined, ClusterMgrL) of
                {ok, ClusterId} ->
                    {ok, ClusterId};
                _ ->
                    not_found
            end;
        Error ->
            Error
    end.


%% @doc Remove a cluster
%% @private
remove_cluster(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, Nodes} ->
            remove_cluster_1(Nodes);
        Error ->
            Error
    end.

%% @private
remove_cluster_1([]) ->
    {error, ?ERROR_COULD_NOT_CONNECT};
remove_cluster_1([Node|Rest]) ->
    {ok, #?SYSTEM_CONF{cluster_id = LocalClusterId}} = leo_cluster_tbl_conf:get(),
    Node_1 = list_to_atom(Node),

    case find_cluster_id_by_manager_node(Node_1) of
        {ok, RetClusterId} ->
            %% Execute removing the cluster info from the remote-cluster
            case catch leo_rpc:call(Node_1, leo_manager_api,
                                    remove_cluster, [LocalClusterId]) of
                ok ->
                    %% Remove the local data of the specified cluster
                    leo_manager_api:remove_cluster(RetClusterId);
                _Error ->
                    remove_cluster_1(Rest)
            end;
        not_found = Cause ->
            Cause;
        {error, Cause} ->
            ?error("remove_cluster_1/1",
                   [{simple_cause, "Failure to leo_mdcr_tbl_cluster_mgr:all/0"},
                    {cause, Cause}]),
            remove_cluster_1(Rest)
    end.


%% @doc Retrieve cluster-statuses
%% @private
cluster_status(Formatter) ->
    Fun = fun() ->
                  case cluster_status_1() of
                      {ok, ResL} ->
                          Formatter:cluster_status(ResL);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_CLUSTER_STAT, Formatter, Fun).

%% @private
cluster_status_1() ->
    case leo_mdcr_tbl_cluster_stat:all() of
        {ok, ResL} ->
            cluster_status_2(ResL, []);
        _ ->
            {error, ?ERROR_COULD_NOT_GET_CLUSTER_INFO}
    end.

%% @private
cluster_status_2([], Acc) ->
    {ok, Acc};
cluster_status_2([#?CLUSTER_STAT{cluster_id = ClusterId,
                                 state      = Status,
                                 updated_at = UpdatedAt
                                }|Rest], Acc) ->
    case leo_mdcr_tbl_cluster_info:get(ClusterId) of
        {ok, #?CLUSTER_INFO{dc_id = DCId,
                            n = N,
                            w = W,
                            r = R,
                            d =D}} ->
            case leo_mdcr_tbl_cluster_member:get(ClusterId) of
                {ok, Rows} ->
                    cluster_status_2(
                      Rest, [
                             [{cluster_id, ClusterId},
                              {dc_id, DCId},
                              {status, Status},
                              {n, N}, {w, W}, {r, R}, {d, D},
                              {members, length(Rows)},
                              {updated_at, UpdatedAt}
                             ]|Acc
                            ]);
                _Error ->
                    _Error
            end;
        _Error ->
            _Error
    end.


%% @doc Retrieve version of the system
%% @private
-spec(version() ->
             {ok, string() | list()}).
version() ->
    case application:get_env(leo_manager, system_version) of
        {ok, Version} ->
            {ok, Version};
        _ ->
            {ok, []}
    end.

%% @doc Retrieve version of every node in a cluster
%% @private
-spec(version_all(binary()) ->
             {ok, list()} | {error, any()}).
version_all(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, ["all"|_]} ->
            Master = atom_to_list(erlang:node()),
            MasterVersion = case application:get_env(leo_manager, system_version) of
                                {ok, Vsn} -> Vsn;
                                undefined -> []
                            end,
            {Slave, SlaveVersion} = case ?env_partner_of_manager_node() of
                                        [] ->
                                            {"not_found", ?ERROR_FAILED_GET_VERSION};
                                        [Partner|_] ->
                                            case rpc:call(Partner, application, get_env, [leo_manager, system_version], ?TIMEOUT_FOR_LEOFSADM) of
                                                {badrpc, _} ->
                                                    {atom_to_list(Partner), ?ERROR_FAILED_GET_VERSION};
                                                {ok, SV} ->
                                                    {atom_to_list(Partner), SV}
                                            end
                                    end,
            Managers = [
                        {"M", Master, MasterVersion},
                        {"M", Slave, SlaveVersion}
                       ],
            Storages = case leo_manager_mnesia:get_storage_nodes_all() of
                           {ok, R1} ->
                               lists:map(fun(N) ->
                                                 Node = N#node_state.node,
                                                 V = case rpc:call(Node, leo_storage_api, get_info, [version], ?TIMEOUT_FOR_LEOFSADM) of
                                                         {badrpc, _} ->
                                                             ?ERROR_FAILED_GET_VERSION;
                                                         Res ->
                                                             Res
                                                     end,
                                                 {?SERVER_TYPE_STORAGE,
                                                  atom_to_list(Node),
                                                  V}
                                         end, R1);
                           _ ->
                               []
                       end,
            Gateways = case leo_manager_mnesia:get_gateway_nodes_all() of
                           {ok, R2} ->
                               lists:map(fun(N) ->
                                                 Node = N#node_state.node,
                                                 V = case rpc:call(Node, leo_gateway_api, get_info, [version], ?TIMEOUT_FOR_LEOFSADM) of
                                                         {badrpc, _} ->
                                                             ?ERROR_FAILED_GET_VERSION;
                                                         Res ->
                                                             Res
                                                     end,
                                                 {?SERVER_TYPE_GATEWAY,
                                                  atom_to_list(Node),
                                                  V}
                                         end, R2);
                           _ ->
                               []
                       end,
            {ok, Managers ++ Storages ++ Gateways};
        {ok, _} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.

%% @doc Exec login
%% @private
-spec(login(binary()) ->
             {ok, #?S3_USER{}, [tuple()]} | {error, any()}).
login(Option) ->
    case ?get_tokens(Option, invalid_args) of
        {ok, [UserId, Password]} ->
            UserIdBin = list_to_binary(UserId),
            PasswordBin = list_to_binary(Password),

            case leo_s3_user:auth(UserIdBin, PasswordBin) of
                {ok, #?S3_USER{} = User} ->
                    case leo_s3_user_credential:get_credential_by_user_id(UserIdBin) of
                        {ok, Credential} ->
                            {ok, User, Credential};
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        _ ->
            {error, invalid_args}
    end.


%% @doc Retrieve state of each node
%% @private
-spec(get_status(binary()) ->
             {ok, any()} | {error, any()}).
get_status(Option) ->
    Token = string:tokens(
              binary_to_list(Option), ?COMMAND_DELIMITER),

    case (erlang:length(Token) == 0) of
        true ->
            %% Retrieve the state of the loacl cluster
            status(node_list);
        false ->
            %% Retrieve the state of a specified node
            [Node|_] = Token,
            status({node_state, Node})
    end.

%% @private
status(node_list) ->
    case leo_cluster_tbl_conf:get() of
        {ok, SystemConf} ->
            Version = case application:get_env(leo_manager, system_version) of
                          {ok, Vsn} ->
                              Vsn;
                          undefined ->
                              []
                      end,
            {ok, {RingHash0, RingHash1}} = leo_redundant_manager_api:checksum(ring),

            S1 = case leo_manager_mnesia:get_storage_nodes_all() of
                     {ok, R1} ->
                         lists:map(fun(N) ->
                                           Node = N#node_state.node,
                                           State = case leo_redundant_manager_api:get_member_by_node(Node) of
                                                       {ok, #member{state = State_1}} ->
                                                           State_1;
                                                       _ ->
                                                           error
                                                   end,
                                           {?SERVER_TYPE_STORAGE,
                                            atom_to_list(Node),
                                            atom_to_list(State),
                                            N#node_state.ring_hash_new,
                                            N#node_state.ring_hash_old,
                                            N#node_state.when_is}
                                   end, R1);
                     _ ->
                         []
                 end,
            S2 = case leo_manager_mnesia:get_gateway_nodes_all() of
                     {ok, R2} ->
                         lists:map(fun(N) ->
                                           {?SERVER_TYPE_GATEWAY,
                                            atom_to_list(N#node_state.node),
                                            atom_to_list(N#node_state.state),
                                            N#node_state.ring_hash_new,
                                            N#node_state.ring_hash_old,
                                            N#node_state.when_is}
                                   end, R2);
                     _ ->
                         []
                 end,
            {ok, {node_list, [{system_config, SystemConf},
                              {version, Version},
                              {ring_hash, [RingHash0, RingHash1]},
                              {nodes, S1 ++ S2}
                             ]}};
        {error, Cause} ->
            {error, Cause}
    end;
status({node_state, Node}) ->
    case leo_manager_api:get_node_status(Node) of
        {ok, {Type, State}} ->
            {ok, {Type, State}};
        {error, Cause} ->
            {error, Cause}
    end;
status(Option) ->
    Tokens = string:tokens(binary_to_list(Option), ?COMMAND_DELIMITER),
    case (Tokens == []) of
        true ->
            %% Reload and store system-conf
            case ?env_mode_of_manager() of
                'master' ->
                    case leo_cluster_tbl_conf:get() of
                        {ok, SystemConf} ->
                            SystemConf;
                        _ ->
                            void
                    end;
                _ ->
                    void
            end,

            %% Retrieve the status
            status(node_list);
        false ->
            [Node|_] = Tokens,
            status({node_state, Node})
    end.


%% @doc Launch the storage cluster
%% @private
-spec(start(port()|null) ->
             ok | {error, any()}).
start(Socket) ->
    case leo_manager_mnesia:get_storage_nodes_all() of
        {ok, _} ->
            case leo_manager_api:get_system_status() of
                ?STATE_STOP ->
                    {ok, SystemConf} = leo_cluster_tbl_conf:get(),

                    case leo_redundant_manager_api:get_members_by_status(?STATE_ATTACHED) of
                        {ok, Nodes} when length(Nodes) >= SystemConf#?SYSTEM_CONF.n ->
                            leo_manager_api:start(Socket);
                        {ok, Nodes} when length(Nodes) < SystemConf#?SYSTEM_CONF.n ->
                            {error, "Attached nodes less than # of replicas"};
                        {error, not_found} ->
                            %% status of all-nodes is 'suspend' or 'restarted'
                            {error, ?ERROR_ALREADY_STARTED};
                        Error ->
                            Error
                    end;
                ?STATE_RUNNING ->
                    {error, ?ERROR_ALREADY_STARTED}
            end;
        _ ->
            {error, ?ERROR_NOT_STARTED}
    end.


%% @doc Detach a storage-node
%% @private
-spec(detach(binary()) ->
             ok | {error, {atom(), string()}} | {error, any()}).
detach(Option) ->
    {ok, SystemConf} = leo_cluster_tbl_conf:get(),

    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Node|_]} ->
            %% target-node is 'attached', then removed it from 'member-table'
            NodeAtom = list_to_atom(Node),
            case leo_redundant_manager_api:get_member_by_node(NodeAtom) of
                {ok, #member{state = ?STATE_ATTACHED}} ->
                    case leo_manager_mnesia:get_storage_node_by_name(NodeAtom) of
                        {ok, NodeState} ->
                            ok = leo_manager_mnesia:delete_storage_node(NodeState),
                            ok = leo_manager_cluster_monitor:demonitor(NodeAtom),
                            ok = leo_redundant_manager_api:delete_member_by_node(NodeAtom),
                            ok;
                        Error ->
                            Error
                    end;
                _ ->
                    %% allow to detach the node?
                    %% if it's ok then execute to detach it
                    N = SystemConf#?SYSTEM_CONF.n,
                    case allow_to_detach_node_1(N) of
                        ok ->
                            case allow_to_detach_node_2(N, NodeAtom) of
                                ok ->
                                    case leo_manager_api:detach(NodeAtom) of
                                        ok ->
                                            ok;
                                        {error, Cause} ->
                                            {error, Node, Cause}
                                    end;
                                Error ->
                                    Error
                            end;
                        Error ->
                            Error
                    end
            end;
        Error ->
            Error
    end.


%% @private
-spec(allow_to_detach_node_1(pos_integer()) ->
             ok | {error, any()}).
allow_to_detach_node_1(N) ->
    case leo_redundant_manager_api:get_members_by_status(?STATE_DETACHED) of
        {ok, Nodes} when length(Nodes) < N ->
            ok;
        {ok,_Nodes} ->
            {error, "Detached nodes greater than or equal # of replicas"};
        {error, not_found} ->
            ok;
        Error ->
            Error
    end.

-spec(allow_to_detach_node_2(pos_integer(), atom()) ->
             ok | {error, any()}).
allow_to_detach_node_2(N, NodeAtom) ->
    IsRunning = case leo_redundant_manager_api:get_member_by_node(NodeAtom) of
                    {ok, #member{state = ?STATE_RUNNING}} ->
                        true;
                    {ok, #member{state = _}} ->
                        false;
                    _ ->
                        {error, "Could not get node-status"}
                end,

    case IsRunning of
        {error, Cause} ->
            {error, Cause};
        _ ->
            case leo_redundant_manager_api:get_members_by_status(?STATE_RUNNING) of
                {ok, Nodes} when IsRunning == false andalso length(Nodes) >= N ->
                    ok;
                {ok, Nodes} when IsRunning == true  andalso length(Nodes) > N ->
                    ok;
                {ok,_Nodes} ->
                    {error, "Running nodes less than # of replicas"};
                {error, not_found} ->
                    {error, "Could not get node-status"};
                _Error ->
                    {error, "Could not get node-status"}
            end
    end.


%% @doc Suspend a storage-node
%% @private
-spec(suspend(binary()) ->
             ok | {error, any()}).
suspend(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Node|_]} ->
            NodeAtom = list_to_atom(Node),
            case leo_redundant_manager_api:get_member_by_node(NodeAtom) of
                {ok, #member{state = ?STATE_RUNNING}} ->
                    leo_manager_api:suspend(NodeAtom);
                _ ->
                    {error, ?ERROR_COULD_NOT_SUSPEND_NODE}
            end;
        Error ->
            Error
    end.


%% @doc Resume a storage-node
%% @private
-spec(resume(binary()) ->
             ok | {error, any()}).
resume(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Node|_]} ->
            NodeAtom = list_to_atom(Node),
            case leo_redundant_manager_api:get_member_by_node(NodeAtom) of
                {ok, #member{state = ?STATE_RUNNING}} ->
                    {error, ?ERROR_COULD_NOT_RESUME_NODE};
                {ok, #member{state = ?STATE_ATTACHED}} ->
                    {error, ?ERROR_COULD_NOT_RESUME_NODE};
                {ok, #member{state = ?STATE_DETACHED}} ->
                    {error, ?ERROR_COULD_NOT_RESUME_NODE};
                {ok, #member{state = ?STATE_STOP}} ->
                    {error, ?ERROR_COULD_NOT_RESUME_NODE};
                _ ->
                    leo_manager_api:resume(NodeAtom)
            end;
        Error ->
            Error
    end.


%% @doc Rollback storage-node from detach to running
%% @private
-spec(rollback(binary()) ->
             ok | {error, any()}).
rollback(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, [Node|_]} ->
            NodeAtom = list_to_atom(Node),
            case leo_redundant_manager_api:get_member_by_node(NodeAtom) of
                {ok, #member{state = ?STATE_RUNNING}} ->
                    {error, "Already running"};
                {ok, #member{state = ?STATE_DETACHED}} ->
                    leo_manager_api:rollback(NodeAtom);
                _ ->
                    {error, ?ERROR_COULD_NOT_ROLLBACK}
            end;
        Error ->
            Error
    end.


%% @doc Purge an object from the cache
%% @private
-spec(purge(binary()) ->
             ok | {error, any()}).
purge(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_PATH) of
        {ok, [Key|_]} ->
            leo_manager_api:purge(Key);
        Error ->
            Error
    end.


%% @doc remove a gateway-node
%% @private
-spec(remove(binary()) ->
             ok | {error, any()}).
remove(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_PATH) of
        {ok, [Node|_]} ->
            leo_manager_api:remove(Node);
        Error ->
            Error
    end.


%% @doc Retrieve the storage stats
%% @private
-spec(du(binary()) ->
             ok | {error, any()}).
du(Option) ->
    case ?get_tokens(Option, ?ERROR_NOT_SPECIFIED_NODE) of
        {ok, Tokens} ->
            Mode = case length(Tokens) of
                       1 ->
                           {summary, lists:nth(1, Tokens)};
                       2 ->
                           case lists:nth(1, Tokens) of
                               "detail" ->
                                   {detail, lists:nth(2, Tokens)};
                               _ ->
                                   {error, ?ERROR_INVALID_ARGS}
                           end;
                       _ -> {error, ?ERROR_INVALID_ARGS}
                   end,

            case Mode of
                {error, _Cause} ->
                    {error, ?ERROR_INVALID_ARGS};
                {Option1, Node1} ->
                    case leo_manager_api:stats(Option1, Node1) of
                        {ok, StatsList} ->
                            {ok, {Option1, StatsList}};
                        {error, Cause} ->
                            {error, Cause}
                    end
            end;
        Error ->
            Error
    end.


%% @doc Compact target node of objects into the object-storages
%% @private
-spec(compact(binary()) ->
             ok | {ok,_} | {error, any()}).
compact(Option) ->
    case ?get_tokens(Option, ?ERROR_NO_CMODE_SPECIFIED) of
        {ok, [Mode, Node|Rest]} ->
            %% command patterns:
            %%   compact start ${storage-node} all | ${num_of_targets} [${num_of_compact_procs}]
            %%   compact suspend ${storage-node}
            %%   compact resume ${storage-node}
            %%   compact status ${storage-node}
            case catch compact(Mode, list_to_atom(Node), Rest) of
                ok ->
                    ok;
                {ok, Status} ->
                    {ok, Status};
                {_, Cause} ->
                    {error, Cause}
            end;
        {ok, [_Mode|_Rest]} ->
            {error, ?ERROR_NOT_SPECIFIED_NODE};
        Error ->
            Error
    end.

%% @private
-spec(compact(string(), atom(), list()) ->
             ok | {ok,_} | {error, any()}).
compact(?COMPACT_START = Mode, Node, [?COMPACT_TARGET_ALL | Rest]) ->
    compact(Mode, Node, 'all', Rest);

compact(?COMPACT_START = Mode, Node, [NumOfTargets0 | Rest]) ->
    case catch list_to_integer(NumOfTargets0) of
        {'EXIT', _} ->
            {error, ?ERROR_INVALID_ARGS};
        NumOfTargets1 ->
            compact(Mode, Node, NumOfTargets1, Rest)
    end;

compact(Mode, Node, _) ->
    leo_manager_api:compact(Mode, Node).


-spec(compact(string(), atom(), atom()|list(), list()) ->
             ok | {ok,_}| {error, any()}).
compact(?COMPACT_START = Mode, Node, NumOfTargets, []) ->
    leo_manager_api:compact(Mode, Node, NumOfTargets, ?env_num_of_compact_proc());

compact(?COMPACT_START = Mode, Node, NumOfTargets, [MaxProc1|_]) ->
    case catch list_to_integer(MaxProc1) of
        {'EXIT',_} ->
            {error, ?ERROR_INVALID_ARGS};
        MaxProc2 ->
            leo_manager_api:compact(Mode, Node, NumOfTargets, MaxProc2)
    end;

compact(_,_,_,_) ->
    {error, ?ERROR_INVALID_ARGS}.


%% @doc Execute data-diagnosis
diagnose_data(Option) ->
    case ?get_tokens(Option, ?ERROR_NO_CMODE_SPECIFIED) of
        {ok, [Node|_]} ->
            leo_manager_api:diagnose_data(list_to_atom(Node));
        Error ->
            Error
    end.


%% @doc Execute data-diagnosis
mq_stats(Option) ->
    case ?get_tokens(Option, ?ERROR_NO_CMODE_SPECIFIED) of
        {ok, [Node|_]} ->
            leo_manager_api:mq_stats(list_to_atom(Node));
        Error ->
            Error
    end.


%% @doc Execute data-diagnosis
mq_suspend(Option) ->
    case ?get_tokens(Option, ?ERROR_NO_CMODE_SPECIFIED) of
        {ok, [Node, MQId|_]} ->
            leo_manager_api:mq_suspend(
              list_to_atom(Node), list_to_atom(MQId));
        Error ->
            Error
    end.


%% @doc Execute data-diagnosis
mq_resume(Option) ->
    case ?get_tokens(Option, ?ERROR_NO_CMODE_SPECIFIED) of
        {ok, [Node, MQId|_]} ->
            leo_manager_api:mq_resume(
              list_to_atom(Node), list_to_atom(MQId));
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @doc Retrieve information of an Assigned object
%% @private
-spec(get_assignments(binary()) ->
             ok | {error, any()}).
get_assignments(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_PATH) of
        {ok, [Key|_]}->
            HasRoutingTable = (leo_redundant_manager_api:checksum(ring) >= 0),
            Key2 = escape_large_obj_sep(Key),

            case catch leo_manager_api:whereis([Key2], HasRoutingTable) of
                {ok, AssignedInfo} ->
                    {ok, AssignedInfo};
                {_, Cause} ->
                    {error, Cause}
            end;
        Error ->
            Error
    end.

%% @private
escape_large_obj_sep(SrcKey) ->
    case string:str(SrcKey, "\\n") of
        0 ->
            SrcKey;
        Index ->
            Len = length(SrcKey),
            DstKey = string:substr(SrcKey, 1, Index - 1),
            CNum = string:substr(SrcKey, Index + 2, Len - Index + 1),
            string:join([DstKey, CNum], "\n")
    end.

%% @doc Recover object(s) by a key/node
%% @private
-spec(recover(pid(), binary()) ->
             ok | {error, any()}).
recover(Socket, Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_PATH) of
        {ok, [?RECOVER_DIR]} ->
            leo_manager_api:rebuild_dir_metadata(Socket, []);
        {ok, [?RECOVER_DIR|Prms]} ->
            leo_manager_api:rebuild_dir_metadata(Socket, Prms);
        {ok, [Op, Key |Rest]} when Rest == [] ->
            HasRoutingTable = (leo_redundant_manager_api:checksum(ring) >= 0),
            Key2 = escape_large_obj_sep(Key),
            case catch leo_manager_api:recover(Op, Key2, HasRoutingTable) of
                ok ->
                    ok;
                {_, Cause} ->
                    {error, Cause}
            end;
        _ ->
            {error, ?ERROR_INVALID_ARGS}
    end.


%% @doc Create a user account (S3)
%% @private
-spec(create_user(binary()) ->
             {ok, [tuple()]} | {error, any()}).
create_user(Option) ->
    Ret = case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
              {ok, [UserId]} ->
                  {ok, {list_to_binary(UserId), <<>>}};
              {ok, [UserId, Password]} ->
                  {ok, {list_to_binary(UserId),
                        list_to_binary(Password)}};
              {ok,_} ->
                  {error, ?ERROR_INVALID_ARGS};
              Error ->
                  Error
          end,

    case Ret of
        {ok, {Arg0, Arg1}} ->
            case leo_s3_user:put(Arg0, Arg1, true) of
                {ok, Keys} ->
                    AccessKeyId     = leo_misc:get_value(access_key_id,     Keys),
                    SecretAccessKey = leo_misc:get_value(secret_access_key, Keys),

                    case Arg1 of
                        <<>> ->
                            ok = leo_s3_user:update(#?S3_USER{id       = Arg0,
                                                              role_id  = ?ROLE_GENERAL,
                                                              password = SecretAccessKey});
                        _ ->
                            void
                    end,
                    {ok, [{access_key_id,     AccessKeyId},
                          {secret_access_key, SecretAccessKey}]};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_ADD_USER}
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Import a user account (S3)
%% @private
-spec(import_user(binary()) ->
             {ok, [tuple()]} | {error, any()}).
import_user(Option) ->
    Ret = case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
              {ok, [UserId, AccessKey, SecretKey]} ->
                  {ok, {false,
                        list_to_binary(UserId),
                        list_to_binary(AccessKey),
                        list_to_binary(SecretKey)}};
              {ok, ["-f", UserId, AccessKey, SecretKey]} ->
                  {ok, {true, %% force update (overwrite the existing records)
                        list_to_binary(UserId),
                        list_to_binary(AccessKey),
                        list_to_binary(SecretKey)}};
              {ok,_} ->
                  {error, ?ERROR_INVALID_ARGS};
              Error ->
                  Error
          end,
    case Ret of
        {ok, {false, UserId_1, AccessKey_1, SecretKey_1}} ->
            case leo_s3_user:import(UserId_1, AccessKey_1, SecretKey_1) of
                {ok, Keys} ->
                    AccessKeyId     = leo_misc:get_value(access_key_id,     Keys),
                    SecretAccessKey = leo_misc:get_value(secret_access_key, Keys),

                    ok = leo_s3_user:update(#?S3_USER{id = UserId_1,
                                                      role_id = ?ROLE_GENERAL,
                                                      password = SecretAccessKey}),

                    {ok, [{access_key_id,     AccessKeyId},
                          {secret_access_key, SecretAccessKey}]};

                %% User ID or Access Key ID Already Exists
                {error, already_exists} ->
                    {error, already_exists};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_ADD_USER}
            end;
        {ok, {true, UserId_1, AccessKey_1, SecretKey_1}} ->
            case leo_s3_user:force_import(UserId_1, AccessKey_1, SecretKey_1) of
                {ok, Keys} ->
                    AccessKeyId     = leo_misc:get_value(access_key_id,     Keys),
                    SecretAccessKey = leo_misc:get_value(secret_access_key, Keys),
                    {ok, [{access_key_id,     AccessKeyId},
                          {secret_access_key, SecretAccessKey}]};
                {error, already_exists} ->
                    {error, already_exists};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_ADD_USER}
            end;
        {error, Cause} ->
            ?error("import_user/1", "Error:~p", [Cause]),
            {error, Cause}
    end.


%% @doc Update user's role-id
%% @private
-spec(update_user_role(binary()) ->
             ok | {error, any()}).
update_user_role(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [UserId, RoleId|_]} ->
            case leo_s3_user:update(#?S3_USER{id       = list_to_binary(UserId),
                                              role_id  = list_to_integer(RoleId),
                                              password = <<>>}) of
                ok ->
                    ok;
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_UPDATE_USER}
            end;
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @doc Update user's password
%% @private
-spec(update_user_password(binary()) ->
             ok | {error, any()}).
update_user_password(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [UserId, Password|_]} ->
            UserIdBin   = list_to_binary(UserId),
            PasswordBin = list_to_binary(Password),
            case leo_s3_user:find_by_id(UserIdBin) of
                {ok, #?S3_USER{role_id = RoleId}} ->
                    leo_s3_user:update(#?S3_USER{id = UserIdBin,
                                                 role_id = RoleId,
                                                 password = PasswordBin});
                not_found ->
                    {error, ?ERROR_USER_NOT_FOUND};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_GET_USER}
            end;
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @doc Remove a user
%% @private
-spec(delete_user(binary()) ->
             ok | {error, any()}).
delete_user(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [UserId|_]} ->
            case leo_s3_user:delete_all_related_records(list_to_binary(UserId)) of
                ok ->
                    ok;
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_REMOVE_USER}
            end;
        Error ->
            Error
    end.


%% @doc Retrieve Users
%% @private
get_users(Formatter) ->
    Fun = fun() ->
                  case get_users_1() of
                      {ok, List} ->
                          Formatter:users(List);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_GET_USERS, Formatter, Fun).


%% @private
-spec(get_users_1() ->
             {ok, list(#credential{})} | {error, any()}).
get_users_1() ->
    case leo_s3_user_credential:find_all_with_role() of
        {ok, Users} ->
            {ok, Users};
        not_found = Cause ->
            {error, Cause};
        Error ->
            Error
    end.


%% @doc Insert an Endpoint into the manager
%% @private
-spec(set_endpoint(binary()) ->
             ok | {error, any()}).
set_endpoint(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [EndPoint|_]} ->
            EndPointBin = list_to_binary(EndPoint),
            leo_manager_api:set_endpoint(EndPointBin);
        Error ->
            Error
    end.


%% @doc Retrieve an Endpoint from the manager
%% @private
get_endpoints(Formatter) ->
    Fun = fun() ->
                  case get_endpoints_1() of
                      {ok, EndPoints} ->
                          Formatter:endpoints(EndPoints);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_GET_ENDPOINTS, Formatter, Fun).

%% @private
-spec(get_endpoints_1() ->
             ok | {error, any()}).
get_endpoints_1() ->
    case leo_s3_endpoint:get_endpoints() of
        {ok, EndPoints} ->
            {ok, EndPoints};
        not_found ->
            {error, ?ERROR_ENDPOINT_NOT_FOUND};
        {error,_Cause} ->
            {error, ?ERROR_COULD_NOT_GET_ENDPOINT}
    end.


%% @doc Remove an Endpoint from the manager
%% @private
-spec(delete_endpoint(binary()) ->
             ok | {error, any()}).
delete_endpoint(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [EndPoint|_]} ->
            EndPointBin = list_to_binary(EndPoint),
            leo_manager_api:delete_endpoint(EndPointBin);
        Error ->
            Error
    end.


%% @doc Insert a Buckets in the manager
%% @private
-spec(add_bucket(binary()) ->
             ok | {error, any()}).
add_bucket(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket, AccessKey]} ->
            BucketBin = list_to_binary(Bucket),
            AccessKeyBin = list_to_binary(AccessKey),
            leo_manager_api:add_bucket(AccessKeyBin, BucketBin);
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @doc Remove a Buckets from the manager
%% @private
-spec(delete_bucket(binary()) ->
             ok | {error, any()}).
delete_bucket(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket, AccessKey]} ->
            BucketBin = list_to_binary(Bucket),
            AccessKeyBin = list_to_binary(AccessKey),
            leo_manager_api:delete_bucket(AccessKeyBin, BucketBin);
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.


%% @doc Retrieve the state of a deletion buckets from the manager
%% @private
-spec(delete_bucket_stats_all() ->
             {ok, [{binary(),[#del_bucket_state{}]}]} | {error, any()}).
delete_bucket_stats_all() ->
    case leo_manager_api:delete_bucket_stats_all() of
        {ok, Stats} ->
            {ok, Stats};
        Other_1 ->
            Other_1
    end.


%% @doc Retrieve the state of a deletion bucket from the manager
%% @private
-spec(delete_bucket_stats(binary()) ->
             {ok, #del_bucket_state{}}  | {error, any()}).
delete_bucket_stats(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket|_]} ->
            BucketBin = list_to_binary(Bucket),
            case leo_manager_api:delete_bucket_stats(BucketBin) of
                {ok, Stats} ->
                    {ok, Stats};
                Other_2 ->
                    Other_2
            end;
        {ok, []} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.

%% @doc Reset(delete) the state of a deletion bucket stats from the manager
%% @private
-spec(reset_delete_bucket_stats(binary()) ->
             ok  | {error, any()}).
reset_delete_bucket_stats(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket|_]} ->
            BucketBin = list_to_binary(Bucket),
            case leo_manager_del_bucket_handler:delete_by_bucket_name(BucketBin) of
                ok ->
                    ok;
                Error0 ->
                    Error0
            end;
        {ok, []} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.

%% @doc Retrieve a Buckets from the manager
%% @private
get_buckets(Formatter) ->
    Fun = fun() ->
                  case get_buckets_1() of
                      {ok, Buckets} ->
                          Formatter:buckets(Buckets);
                      {error, Cause} ->
                          Formatter:error(Cause)
                  end
          end,
    invoke(?CMD_GET_BUCKETS, Formatter, Fun).

-spec(get_buckets_1() ->
             {ok,[#?BUCKET{}]} | {error, any()}).
get_buckets_1() ->
    case catch leo_s3_bucket:find_all_including_owner() of
        {ok, Buckets} ->
            {ok, Buckets};
        not_found ->
            {error, ?ERROR_BUCKET_NOT_FOUND};
        {_,_Cause} ->
            {error, ?ERROR_COULD_NOT_GET_BUCKET}
    end.


%% @doc Retrieve a Buckets from the manager
%% @private
-spec(get_bucket_by_access_key(binary()) ->
             ok | {error, any()}).
get_bucket_by_access_key(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [AccessKey|_]} ->
            leo_s3_bucket:find_buckets_by_id(list_to_binary(AccessKey));
        Error ->
            Error
    end.


%% @doc Change owner of a bucket
%% @private
-spec(change_bucket_owner(binary()) ->
             ok | {error, any()}).
change_bucket_owner(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket, NewAccessKeyId]} ->
            case leo_s3_bucket:change_bucket_owner(
                   list_to_binary(NewAccessKeyId),
                   list_to_binary(Bucket)) of
                ok ->
                    ok;
                not_found ->
                    {error, ?ERROR_BUCKET_NOT_FOUND};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_UPDATE_BUCKET}
            end;
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.

%% #doc Set redundancy method of a bucket
set_redundancy_method(Option) ->
    Ret = case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
              {ok, [Bucket, AccessKeyId, ?RED_METHOD_STR_COPY = RedMethod]} ->
                  {ok, [Bucket, AccessKeyId, RedMethod, "0", "0"]};
              {ok, [Bucket, AccessKeyId, ?RED_METHOD_STR_EC = RedMethod, ECParam_K, ECParam_M]} ->
                  {ok, [Bucket, AccessKeyId, RedMethod, ECParam_K, ECParam_M]};
              {ok,_} ->
                  {error, ?ERROR_INVALID_ARGS};
              Error ->
                  Error
          end,

    case Ret of
        {ok, [Bucket_1, AccessKeyId_1,
              RedMethod_1, ECParam_K_1, ECParam_M_1]} ->
            AccessKeyId_2 = list_to_binary(AccessKeyId_1),
            Bucket_2 = list_to_binary(Bucket_1),
            StrToInt = fun(_StrInt) ->
                               case catch list_to_integer(_StrInt) of
                                   {'EXIT',_} ->
                                       void;
                                   _Int ->
                                       _Int
                               end
                       end,
            ECParam_K_2 = StrToInt(ECParam_K_1),
            ECParam_M_2 = StrToInt(ECParam_M_1),

            case leo_s3_bucket:set_redundancy_method(
                   AccessKeyId_2, Bucket_2, RedMethod_1,
                   'vandrs', {ECParam_K_2, ECParam_M_2}) of
                ok ->
                    leo_manager_api:update_bucket(Bucket_2);
                not_found ->
                    {error, ?ERROR_BUCKET_NOT_FOUND};
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_UPDATE_BUCKET}
            end;
        Error_1 ->
            Error_1
    end.


%% @doc Update ACLs of the specified bucket with the Canned ACL in the manager
%% @private
-spec(update_acl(binary()) ->
             ok | {error, any()}).
update_acl(Option) ->
    case ?get_tokens(Option, ?ERROR_INVALID_ARGS) of
        {ok, [Bucket, AccessKey, Permission]} ->
            BucketBin = list_to_binary(Bucket),
            case leo_s3_bucket:find_bucket_by_name(BucketBin) of
                {ok,_} ->
                    case leo_manager_api:update_acl(Permission,
                                                    list_to_binary(AccessKey),
                                                    BucketBin) of
                        ok ->
                            ok;
                        _Error ->
                            {error, ?ERROR_COULD_NOT_UPDATE_BUCKET}
                    end;
                not_found ->
                    {error, ?ERROR_BUCKET_NOT_FOUND};
                _ ->
                    {error, ?ERROR_COULD_NOT_UPDATE_BUCKET}
            end;
        {ok,_} ->
            {error, ?ERROR_INVALID_ARGS};
        Error ->
            Error
    end.
