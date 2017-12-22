%%====================================================================
%%
%% LeoStorage
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
%% -------------------------------------------------------------------
%% LeoFS Storage - MQ Client
%% @doc
%% @end
%%====================================================================
-module(leo_storage_mq).

-behaviour(leo_mq_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/1, start/2,
         publish/2, publish/3, publish/4, publish/5, publish/6]).
-export([init/0, handle_call/1, handle_call/3]).

-define(SLASH, "/").


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc create queues and launch mq-servers.
-spec(start(RootPath) ->
             ok | {error, any()} when RootPath::binary()).
start(RootPath) ->
    start(leo_storage_sup, RootPath).

-spec(start(RefSup, RootPath) ->
             ok | {error, any()} when RefSup::pid(),
                                      RootPath::binary()).
start(RefSup, RootPath) ->
    %% launch mq-sup under storage-sup
    RefMqSup =
        case whereis(leo_mq_sup) of
            undefined ->
                ChildSpec = {leo_mq_sup,
                             {leo_mq_sup, start_link, []},
                             permanent, 2000, supervisor, [leo_mq_sup]},
                {ok, Pid} = supervisor:start_child(RefSup, ChildSpec),
                Pid;
            Pid ->
                Pid
        end,

    %% launch queue-processes
    RootPath_1 =
        case (string:len(RootPath) == string:rstr(RootPath, ?SLASH)) of
            true  -> RootPath;
            false -> RootPath ++ ?SLASH
        end,

    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER,
                                     [named_table, public, {read_concurrency, true}]),
    DelBucketQueues = ?del_dir_queue_list(),
    start_1([{?QUEUE_ID_PER_OBJECT,        ?MSG_PATH_PER_OBJECT},
             {?QUEUE_ID_SYNC_BY_VNODE_ID,  ?MSG_PATH_SYNC_VNODE_ID},
             {?QUEUE_ID_REBALANCE,         ?MSG_PATH_REBALANCE},
             {?QUEUE_ID_ASYNC_DELETION,    ?MSG_PATH_ASYNC_DELETION},
             {?QUEUE_ID_RECOVERY_NODE,     ?MSG_PATH_RECOVERY_NODE},
             {?QUEUE_ID_SYNC_OBJ_WITH_DC,  ?MSG_PATH_SYNC_OBJ_WITH_DC},
             {?QUEUE_ID_COMP_META_WITH_DC, ?MSG_PATH_COMP_META_WITH_DC},
             {?QUEUE_ID_REQ_DEL_DIR,       ?MSG_PATH_REQ_DEL_DIR}
            ] ++ DelBucketQueues, RefMqSup, RootPath_1).

%% @private
start_1([],_,_) ->
    ok;
start_1([{Id, Path}|Rest], Sup, Root) ->
    leo_mq_api:new(Sup, Id, [{?MQ_PROP_MOD, ?MODULE},
                             {?MQ_PROP_FUN, ?MQ_SUBSCRIBE_FUN},
                             {?MQ_PROP_ROOT_PATH, Root ++ Path},
                             {?MQ_PROP_DB_NAME, ?env_mq_backend_db()},
                             {?MQ_PROP_DB_PROCS, ?env_num_of_mq_procs()},
                             {?MQ_PROP_CNS_PROCS_PER_DB, 1},
                             {?MQ_PROP_BATCH_MSGS_MAX, ?env_mq_num_of_batch_process_max()},
                             {?MQ_PROP_BATCH_MSGS_REG, ?env_mq_num_of_batch_process_reg()},
                             {?MQ_PROP_INTERVAL_MAX, ?env_mq_interval_between_batch_procs_max()},
                             {?MQ_PROP_INTERVAL_REG, ?env_mq_interval_between_batch_procs_reg()}
                            ]),
    start_1(Rest, Sup, Root).


%% @doc Input a message into the queue.
-spec(publish(mq_id(), atom()|binary()) ->
             ok | {error, any()}).
publish(?QUEUE_ID_RECOVERY_NODE = Id, Node) ->
    KeyBin = term_to_binary(Node),
    MsgBin = term_to_binary(
               #recovery_node_message{id = leo_date:clock(),
                                      node = Node,
                                      timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MsgBin);

publish(?QUEUE_ID_ASYNC_DELETION = Id, #?METADATA{addr_id = AddrId,
                                                  key = Key} = Metadata) ->
    KeyBin = term_to_binary({AddrId, Key}),
    MessageBin = term_to_binary(
                   #?MSG_ASYNC_DELETION{id = leo_date:clock(),
                                        addr_id = AddrId,
                                        key = Key,
                                        meta = Metadata,
                                        timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);
publish(_,_) ->
    {error, badarg}.

-spec(publish(mq_id(), any(), any()) ->
             ok | {error, any()}).
publish(?QUEUE_ID_PER_OBJECT = Id, #?METADATA{addr_id = AddrId,
                                              key = Key}= Metadata, ErrorType) ->
    KeyBin = term_to_binary({ErrorType, Key}),
    MessageBin = term_to_binary(
                   #?MSG_INCONSISTENT_DATA{id = leo_date:clock(),
                                           type = ErrorType,
                                           addr_id = AddrId,
                                           key = Key,
                                           meta = Metadata,
                                           timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);

publish(?QUEUE_ID_SYNC_BY_VNODE_ID = Id, VNodeId, Node) ->
    KeyBin = term_to_binary({VNodeId, Node}),
    MessageBin = term_to_binary(
                   #sync_unit_of_vnode_message{id = leo_date:clock(),
                                               vnode_id = VNodeId,
                                               node = Node,
                                               timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);

publish(?QUEUE_ID_ASYNC_DELETION = Id, AddrId, Key) ->
    case leo_storage_handler_object:head(AddrId, Key, false) of
        {ok, Metadata} ->
            publish(Id, Metadata);
        not_found ->
            ok;
        {error, Cause} ->
            ?error("publish/3",
                   [{addr_id, AddrId}, {key, Key},
                    {cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_META}
    end;

publish(?QUEUE_ID_SYNC_OBJ_WITH_DC, AddrId, Key) ->
    publish(?QUEUE_ID_SYNC_OBJ_WITH_DC, undefined, AddrId, Key);

publish(?QUEUE_ID_COMP_META_WITH_DC = Id, ClusterId, AddrAndKeyList) ->
    KeyBin = term_to_binary(ClusterId),
    MessageBin = term_to_binary(
                   #comparison_metadata_with_dc{id = leo_date:clock(),
                                                cluster_id = ClusterId,
                                                list_of_addrid_and_key = AddrAndKeyList,
                                                timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);

publish(?QUEUE_ID_REQ_DEL_DIR = Id, Node, Directory) ->
    KeyBin = term_to_binary({Node, Directory}),
    MsgBin = term_to_binary(#delete_dir{
                               id = leo_date:clock(),
                               node = Node,
                               dir = Directory,
                               timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MsgBin);
publish(_,_,_) ->
    {error, badarg}.

-spec(publish(mq_id(), any(), any(), any()) ->
             ok | {error, any()}).
publish(?QUEUE_ID_PER_OBJECT = Id, AddrId, Key, ErrorType) ->
    case leo_storage_handler_object:head(AddrId, Key, false) of
        {ok, Metadata} ->
            publish(Id, Metadata, ErrorType);
        not_found ->
            ok;
        {error, Cause} ->
            ?error("publish/4",
                   [{addr_id, AddrId}, {key, Key},
                    {cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_META}
    end;

publish(?QUEUE_ID_SYNC_OBJ_WITH_DC = Id, ClusterId, AddrId, Key) ->
    KeyBin = term_to_binary({ClusterId, AddrId, Key}),
    MessageBin  = term_to_binary(
                    #inconsistent_data_with_dc{id = leo_date:clock(),
                                               cluster_id = ClusterId,
                                               addr_id = AddrId,
                                               key = Key,
                                               del = 0,
                                               timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);

publish({?QUEUE_ID_DEL_DIR, WorkerId}, AddrId, Key, EnqueuedAt) ->
    KeyBin = term_to_binary({AddrId, Key}),
    MsgBin = term_to_binary(#?MSG_ASYNC_DELETION{
                                id = leo_date:clock(),
                                addr_id = AddrId,
                                key = Key,
                                %% To compare it with its latest metadata
                                %% See `compare_between_metadatas/2`
                                meta = #?METADATA{addr_id = AddrId,
                                                  key = Key,
                                                  clock = EnqueuedAt},
                                timestamp = leo_date:now()}),

    leo_mq_api:publish(WorkerId, KeyBin, MsgBin);
publish(_,_,_,_) ->
    {error, badarg}.

-spec(publish(mq_id(), any(), any(), any(), any()) ->
             ok | {error, any()}).
publish(?QUEUE_ID_REBALANCE = Id, Node, VNodeId, AddrId, Key) ->
    %% Check Key under del-bucket/directory or not
    case leo_storage_handler_object:is_key_under_del_dir(Key) of
        true ->
            ok;
        false ->
            %% Enqueue a message of the rebalance
            Hash = erlang:crc32(term_to_binary({Node, AddrId, Key})),
            KeyBin = term_to_binary({Hash, Node, AddrId, Key}),
            MessageBin = term_to_binary(
                           #rebalance_message{id = leo_date:clock(),
                                              vnode_id = VNodeId,
                                              addr_id = AddrId,
                                              key = Key,
                                              node = Node,
                                              timestamp = leo_date:now()}),
            Table = ?TBL_REBALANCE_COUNTER,
            case ets_lookup(Table, VNodeId) of
                {ok, 0} ->
                    ets:insert(Table, {VNodeId, 0});
                _Other ->
                    void
            end,
            ok = increment_counter(Table, VNodeId),
            leo_mq_api:publish(Id, KeyBin, MessageBin)
    end;

publish(?QUEUE_ID_SYNC_OBJ_WITH_DC = Id, ClusterId, AddrId, Key, Del) ->
    KeyBin = term_to_binary({ClusterId, AddrId, Key}),
    MessageBin  = term_to_binary(
                    #inconsistent_data_with_dc{id = leo_date:clock(),
                                               cluster_id = ClusterId,
                                               addr_id = AddrId,
                                               key = Key,
                                               del = Del,
                                               timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);
publish(_,_,_,_,_) ->
    {error, badarg}.

publish(?QUEUE_ID_PER_OBJECT = Id, AddrId, Key, SyncNode, true = IsForceSync, ErrorType) ->
    KeyBin = term_to_binary({ErrorType, Key}),
    MessageBin = term_to_binary(
                   #?MSG_INCONSISTENT_DATA{id = leo_date:clock(),
                                           type = ErrorType,
                                           addr_id = AddrId,
                                           key = Key,
                                           sync_node = SyncNode,
                                           is_force_sync = IsForceSync,
                                           timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);
publish(?QUEUE_ID_PER_OBJECT = Id, AddrId, Key, SyncNode, IsForceSync, ErrorType) ->
    case leo_storage_handler_object:head(AddrId, Key, false) of
        {ok, Metadata} ->
            KeyBin = term_to_binary({ErrorType, Key}),
            MessageBin = term_to_binary(
                           #?MSG_INCONSISTENT_DATA{id = leo_date:clock(),
                                                   type = ErrorType,
                                                   addr_id = AddrId,
                                                   key = Key,
                                                   meta = Metadata,
                                                   sync_node = SyncNode,
                                                   is_force_sync = IsForceSync,
                                                   timestamp = leo_date:now()}),
            leo_mq_api:publish(Id, KeyBin, MessageBin);
        not_found ->
            ok;
        {error, Cause} ->
            ?error("publish/4",
                   [{addr_id, AddrId}, {key, Key},
                    {cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_META}
    end;
publish(_,_,_,_,_,_) ->
    {error, badarg}.


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------
%% @doc Initializer
-spec(init() -> ok | {error, any()}).
init() ->
    ok.


%% @doc Subscribe a message from the queue.
%% @private
-spec(handle_call({consume, any() | mq_id() , any() | binary()}) ->
             ok | {error, any()}).
handle_call({consume, ?QUEUE_ID_PER_OBJECT, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        {'EXIT',_} ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_PER_OBJECT},
                   {cause, invalid_data_format}]),
            ok;
        Term ->
            case ?transform_inconsistent_data_message(Term) of
                {ok, #?MSG_INCONSISTENT_DATA{addr_id = AddrId,
                                             key = Key,
                                             meta = Metadata,
                                             sync_node = SyncNode,
                                             is_force_sync = IsForceSync}} ->
                    case (IsForceSync == true andalso
                          SyncNode /= undefined) of
                        true ->
                            send_object_to_remote_node(SyncNode, AddrId, Key);
                        false ->
                            case compare_between_metadatas(Metadata, false) of
                                true ->
                                    case correct_redundancies(Key) of
                                        ok ->
                                            ok;
                                        {error, Cause = not_found} ->
                                            ?warn("handle_call/1 - consume",
                                                  [{qid, ?QUEUE_ID_PER_OBJECT},
                                                   {addr_id, AddrId},
                                                   {key, Key}, {cause, Cause}]),
                                            ok;
                                        {error, Cause} ->
                                            ?debug("handle_call/1 - consume",
                                                   [{addr_id, AddrId},
                                                    {key, Key}, {cause, Cause}]),
                                            {error, Cause}
                                    end;
                                false ->
                                    ok;
                                {error, Cause} ->
                                    {error, Cause}
                            end;
                        {error,_Error} ->
                            ?warn("handle_call/1 - consume",
                                  [{qid, ?QUEUE_ID_PER_OBJECT},
                                   {cause, invalid_data_format}]),
                            ok
                    end
            end
    end;

handle_call({consume, ?QUEUE_ID_SYNC_BY_VNODE_ID, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #sync_unit_of_vnode_message{vnode_id = ToVNodeId,
                                    node = Node} ->
            {ok, Res} = leo_redundant_manager_api:range_of_vnodes(ToVNodeId),
            {ok, {CurRingHash, _PrevRingHash}} =
                leo_redundant_manager_api:checksum(?CHECKSUM_RING),
            ok = sync_vnodes(Node, CurRingHash, Res),
            notify_rebalance_message_to_manager(ToVNodeId);
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_SYNC_BY_VNODE_ID},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, ?QUEUE_ID_REBALANCE, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #rebalance_message{key = Key} = Msg ->
            %% Check Key under del-bucket/directory or not
            case leo_storage_handler_object:is_key_under_del_dir(Key) of
                true ->
                    ok;
                false ->
                    rebalance_1(Msg)
            end;
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_REBALANCE},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, ?QUEUE_ID_ASYNC_DELETION, MessageBin}) ->
    remove_objects_under_dir(MessageBin);

handle_call({consume, ?QUEUE_ID_RECOVERY_NODE, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #recovery_node_message{node = Node} ->
            recover_node(Node);
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_RECOVERY_NODE},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, ?QUEUE_ID_SYNC_OBJ_WITH_DC, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #inconsistent_data_with_dc{} = Msg ->
            fix_consistency_between_clusters(Msg);
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_SYNC_OBJ_WITH_DC},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, ?QUEUE_ID_COMP_META_WITH_DC, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #comparison_metadata_with_dc{cluster_id = ClusterId,
                                     list_of_addrid_and_key = AddrAndKeyList} ->
            %% @doc - condition: if state of a remote-cluster is not 'running',
            %%                   then this queue is removed
            case leo_mdcr_tbl_cluster_stat:find_by_cluster_id(ClusterId) of
                {ok, #?CLUSTER_STAT{state = ?STATE_RUNNING}} ->
                    %% re-compare metadatas
                    leo_storage_handler_sync:send_addrid_and_key_to_remote(
                      ClusterId, AddrAndKeyList);
                _ ->
                    ok
            end;
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_COMP_META_WITH_DC},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, ?QUEUE_ID_REQ_DEL_DIR, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        #delete_dir{node = Node,
                    dir = Directory} ->
            case rpc:call(Node, leo_storage_handler_del_directory, enqueue,
                          [?TYPE_DEL_DIR, Directory], ?DEF_REQ_TIMEOUT) of
                ok ->
                    ?debug("leo_mq delete directory", [{dir, Directory}]),
                    ok;
                Cause ->
                    {error, Cause}
            end;
        _ ->
            ?warn("handle_call/1 - consume",
                  [{qid, ?QUEUE_ID_REQ_DEL_DIR},
                   {cause, invalid_data_format}]),
            ok
    end;

handle_call({consume, MQId, MessageBin}) ->
    case lists:member(MQId, ?del_dir_id_list()) of
        true ->
            %% Need to handle the returun value of 'remove_objects_under_dir/1'
            %% because there is a case that an item doesn't get inserted into 'leo_async_deletion_queue'
            %% when leo_storage_handler_object:head in compare_between_metadatas failed.
            remove_objects_under_dir(MessageBin);
        false ->
            ok
    end;
handle_call(_) ->
    ok.
handle_call(_,_,_) ->
    ok.


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS-1
%%--------------------------------------------------------------------
%% @doc synchronize by vnode-id.
%% @private
-spec(recover_node(Node) ->
             ok when Node::node()).
recover_node(Node) ->
    Callback = recover_node_callback(Node),
    _ = leo_object_storage_api:fetch_by_addr_id(0, Callback),
    ok.

%% @private
-spec(recover_node_callback(Node) ->
             any() when Node::node()).
recover_node_callback(Node) ->
    fun(K, V, Acc) ->
            Metadata_1 = binary_to_term(V),
            Metadata_2 = leo_object_storage_transformer:transform_metadata(Metadata_1),
            #?METADATA{addr_id = AddrId} = Metadata_2,

            %% Retrieve redundant-nodes from the redundant-manager(RING),
            %% then if the recovery-target-node and "this node"
            %% are included in retrieved redundant-nodes, "the file" will be recovered
            %% by the MQ.
            case leo_redundant_manager_api:get_redundancies_by_addr_id(put, AddrId) of
                {ok, #redundancies{nodes = Redundancies}} ->
                    RedundantNodes = [N || #redundant_node{node = N} <- Redundancies],
                    ok = recover_node_callback_1(AddrId, K, Node, RedundantNodes),
                    Acc;
                _Other ->
                    Acc
            end
    end.

%% @private
recover_node_callback_1(_,_,_,[]) ->
    ok;
recover_node_callback_1(AddrId, Key, Node, RedundantNodes) ->
    case lists:member(Node, RedundantNodes) of
        true ->
            case lists:delete(Node, RedundantNodes) of
                [] ->
                    ok;
                RedundantNodes_1 ->
                    recover_node_callback_2(lists:member(erlang:node(), RedundantNodes_1), AddrId, Key, Node)
            end;
        false ->
            ok
    end.

%% @private
recover_node_callback_2(false,_AddrId,_Key,_FixedNode) ->
    %% the object is not belonged to this node
    ok;
recover_node_callback_2(true, AddrId, Key, FixedNode) ->
    %% the object is belonged
    %% then have to try to send the object regardress of the liveness of others,
    %% because there is no guarantee others have the object stored properly
    %% as reported on https://github.com/leo-project/leofs/issues/880
    ?MODULE:publish(?QUEUE_ID_PER_OBJECT,
                    AddrId, Key, FixedNode, true,
                    ?ERR_TYPE_RECOVER_DATA).

%% @doc Send object to a remote-node
%% @private
-spec(send_object_to_remote_node(Node, AddrId, Key) ->
             ok | {error, Cause} when Node::node(),
                                      AddrId::non_neg_integer(),
                                      Key::binary(),
                                      Cause::any()).
send_object_to_remote_node(Node, AddrId, Key) ->
    case catch leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            case catch binary_to_term(MetaBin) of
                #?METADATA{del = ?DEL_TRUE} = Meta ->
                    %% for objects deleted
                    case rpc:call(Node, leo_sync_local_cluster, store,
                          [Meta, <<>>], ?DEF_REQ_TIMEOUT) of
                        ok ->
                            ok;
                        {error, inconsistent_obj} ->
                            ?MODULE:publish(?QUEUE_ID_PER_OBJECT,
                                            AddrId, Key, ?ERR_TYPE_RECOVER_DATA);
                        _ ->
                            ?MODULE:publish(?QUEUE_ID_PER_OBJECT, AddrId, Key,
                                            Node, true, ?ERR_TYPE_RECOVER_DATA)
                    end;
                #?METADATA{del = ?DEL_FALSE} = Meta ->
                    %% for objects existing
                    Ref = make_ref(),
                    case leo_storage_handler_object:get({Ref, Key}) of
                        {ok, Ref, Meta, Bin} ->
                            case rpc:call(Node, leo_sync_local_cluster, store,
                                          [Meta, Bin], ?DEF_REQ_TIMEOUT) of
                                ok ->
                                    ok;
                                {error, inconsistent_obj} ->
                                    ?MODULE:publish(?QUEUE_ID_PER_OBJECT,
                                                    AddrId, Key, ?ERR_TYPE_RECOVER_DATA);
                                _ ->
                                    ?MODULE:publish(?QUEUE_ID_PER_OBJECT, AddrId, Key,
                                                    Node, true, ?ERR_TYPE_RECOVER_DATA)
                            end;
                        {error, Ref, Cause} ->
                            {error, Cause};
                        _Other ->
                            {error, invalid_response}
                    end;
                {'EXIT', Cause} ->
                    %% unexpected case now @vstax seems to face
                    %% invalid binary may be stored on leveldb if so have to vet the raw binary
                    %% that can be retrieved from error outputs below.
                    ?error("send_object_to_remote_node/3 - binary_to_term",
                          [{node, Node},
                           {addr_id, AddrId},
                           {key, Key},
                           {cause, Cause}]),
                    {error, Cause}
            end;
        {'EXIT', Cause} ->
            %% unexpected case now @vstax seems to face
            %% invalid binary may be stored on leveldb so we have to vet the raw binary
            %% that can be retrieved from error outputs below.
            ?error("send_object_to_remote_node/3 - leo_object_storage_api:head",
                   [{node, Node},
                    {addr_id, AddrId},
                    {key, Key},
                    {cause, Cause}]),
            {error, Cause};
        not_found ->
            %% no data to send remote nodes
            ok;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc synchronize by vnode-id.
%% @private
-spec(sync_vnodes(Node, RingHash, ListOfFromToAddrId) ->
             ok when Node::node(),
                     RingHash::integer(),
                     FromAddrId::integer(),
                     ToAddrId::integer(),
                     ListOfFromToAddrId::[{FromAddrId, ToAddrId}]).
sync_vnodes(_, _, []) ->
    ok;
sync_vnodes(Node, RingHash, [{FromAddrId, ToAddrId}|T]) ->
    Callback = sync_vnodes_callback(Node, FromAddrId, ToAddrId),
    catch leo_object_storage_api:fetch_by_addr_id(FromAddrId, Callback),
    catch notify_message_to_manager(?env_manager_nodes(leo_storage), ToAddrId, erlang:node()),
    sync_vnodes(Node, RingHash, T).

%% @private
-spec(sync_vnodes_callback(Node, FromAddrId, ToAddrId) ->
             any() when Node::node(),
                        FromAddrId::integer(),
                        ToAddrId::integer()).
sync_vnodes_callback(Node, FromAddrId, ToAddrId)->
    fun(_K, V, Acc) ->
            %% Note: An object of copy is NOT equal current ring-hash.
            %%       Then a message in the rebalance-queue.
            #?METADATA{addr_id = AddrId,
                       key = Key} = binary_to_term(V),

            case (AddrId >= FromAddrId andalso
                  AddrId =< ToAddrId) of
                true ->
                    case catch leo_redundant_manager_api:get_redundancies_by_key(Key) of
                        {'EXIT',_Cause} ->
                            Acc;
                        {ok, #redundancies{nodes = Redundancies}} ->
                            Nodes = [N || #redundant_node{node = N} <- Redundancies],
                            case lists:member(Node, Nodes) of
                                true ->
                                    publish(?QUEUE_ID_REBALANCE, Node,
                                            ToAddrId, AddrId, Key),
                                    Acc;
                                false ->
                                    Acc
                            end,
                            Acc;
                        _ ->
                            Acc
                    end;
                false ->
                    Acc
            end
    end.


%% @doc Remove a node from redundancies
%% @private
-spec(delete_node_from_redundancies(Redundancies, Node, AccRedundancies) ->
             {ok, AccRedundancies} when Redundancies::[#redundant_node{}],
                                        Node::node(),
                                        AccRedundancies::[#redundant_node{}]).
delete_node_from_redundancies([],_,Acc) ->
    {ok, lists:reverse(Acc)};
delete_node_from_redundancies([#redundant_node{node = Node}|Rest], Node, Acc) ->
    delete_node_from_redundancies(Rest, Node, Acc);
delete_node_from_redundancies([RedundantNode|Rest], Node, Acc) ->
    delete_node_from_redundancies(Rest, Node, [RedundantNode|Acc]).


%% @doc Find a node from redundancies
%% @private
-spec(find_node_from_redundancies(Redundancies, Node) ->
             Ret when Redundancies::[#redundant_node{}],
                      Node::node(),
                      Ret::boolean()).
find_node_from_redundancies([],_) ->
    false;
find_node_from_redundancies([#redundant_node{node = Node}|_], Node) ->
    true;
find_node_from_redundancies([_|Rest], Node) ->
    find_node_from_redundancies(Rest, Node).


%% @doc Notify a message to manager node(s)
%% @private
-spec(notify_message_to_manager(ManagerNodes, VNodeId, Node) ->
             ok | {error, Cause} when ManagerNodes::[node()],
                                      VNodeId::integer(),
                                      Node::node(),
                                      Cause::any()).
notify_message_to_manager([],_VNodeId,_Node) ->
    {error, 'fail_notification'};
notify_message_to_manager([Manager|T], VNodeId, Node) ->
    Res = case rpc:call(list_to_atom(Manager), leo_manager_api, notify,
                        [synchronized, VNodeId, Node], ?DEF_REQ_TIMEOUT) of
              ok ->
                  ok;
              {_, Cause} ->
                  ?warn("notify_message_to_manager/3",
                        [{vnode_id, VNodeId},
                         {node, Node}, {cause, Cause}]),
                  {error, Cause};
              timeout = Cause ->
                  {error, Cause}
          end,

    case Res of
        ok ->
            ok;
        _Error ->
            notify_message_to_manager(T, VNodeId, Node)
    end.


%% @doc correct_redundancies/1 - first.
%% @private
-spec(correct_redundancies(Key) ->
             ok | {error, Cause} when Key::binary(),
                                      Cause::any()).
correct_redundancies(Key) ->
    case leo_redundant_manager_api:get_redundancies_by_key(Key) of
        {ok, #redundancies{nodes = Redundancies,
                           id = AddrId}} ->
            correct_redundancies_1(Key, AddrId, Redundancies, [], []);
        {error, Cause} ->
            ?warn("correct_redundancies/1",
                  [{key, Key}, {cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_REDUNDANCY}
    end.

%% @doc correct_redundancies_1/5 - next.
%% @private
-spec(correct_redundancies_1(binary(), integer(), list(), list(), list()) ->
             ok | {error, any()}).
correct_redundancies_1(_Key,_AddrId, [], [], _ErrorNodes) ->
    {error, not_found};
correct_redundancies_1(_Key,_AddrId, [], Metadatas, ErrorNodes) ->
    correct_redundancies_2(Metadatas, ErrorNodes);
correct_redundancies_1(Key, AddrId, [#redundant_node{node = Node}|T], Metadatas, ErrorNodes) ->
    %% NOTE:
    %% If remote-node status is NOT 'running',
    %%     this function cannot operate 'rpc-call'.
    case leo_redundant_manager_api:get_member_by_node(Node) of
        {ok, #member{state = ?STATE_RUNNING}} ->
            %% Retrieve a metadata from remote-node
            %% invoke head with NO retry option
            RPCKey = rpc:async_call(Node, leo_object_storage_api,
                                    head_with_check_avs, [{AddrId, Key}, check_header]),

            case rpc:nb_yield(RPCKey, ?DEF_REQ_TIMEOUT) of
                {value, {ok, MetaBin}} ->
                    Metadata = binary_to_term(MetaBin),
                    correct_redundancies_1(Key, AddrId, T,
                                           [{Node, Metadata}|Metadatas], ErrorNodes);
                _Error ->
                    correct_redundancies_1(Key, AddrId, T,
                                           Metadatas, [Node|ErrorNodes])
            end;
        {ok, #member{state = ?STATE_DETACHED}} ->
            correct_redundancies_1(Key, AddrId, T, Metadatas, ErrorNodes);
        _Other ->
            correct_redundancies_1(Key, AddrId, T, Metadatas, [Node|ErrorNodes])
    end.

%% @doc correct_redundancies_2/3
%% @private
-spec(correct_redundancies_2(ListOfMetadatas, ErrorNodes) ->
             ok | {error, any()} when ListOfMetadatas::[#?METADATA{}],
                                      ErrorNodes::[any()]).
correct_redundancies_2(ListOfMetadatas, ErrorNodes) ->
    %% Retrieve latest metadata of an object to fix its inconsistency
    H = case (erlang:length(ListOfMetadatas) == 1) of
            true ->
                erlang:hd(ListOfMetadatas);
            false ->
                MaxClock = lists:max([M#?METADATA.clock
                                      || {_,M} <- ListOfMetadatas]),
                {_,RetL} = lists:foldl(
                             fun({_,#?METADATA{clock = Clock}} = M,
                                 {MaxClock_1, Acc}) when Clock == MaxClock_1 ->
                                     {MaxClock_1, [M|Acc]};
                                (_, {MaxClock_1, Acc}) ->
                                     {MaxClock_1, Acc}
                             end, {MaxClock, []}, ListOfMetadatas),
                erlang:hd(RetL)
        end,
    {_, Metadata} = H,

    %% If 'metadata' contains 'num_of_replicas > 0',
    %% it is adopted instead of the local 'number of replicas'
    {_Dest, CorrectNodes, InconsistentNodes} =
        lists:foldl(
          fun({Node,_Metadata}, {{DestNode,_Metadata} = Dest, C, R}) when Node =:= DestNode ->
                  {Dest, [Node|C], R};
             ({Node, #?METADATA{clock = Clock}},
              {{DestNode, #?METADATA{clock = DestClock}} = Dest, C, R}) when Node  =/= DestNode,
                                                                             Clock =:= DestClock ->
                  {Dest, [Node|C], R};
             ({Node, #?METADATA{clock = Clock}},
              {{DestNode, #?METADATA{clock = DestClock}} = Dest, C, R}) when Node  =/= DestNode,
                                                                             Clock =/= DestClock ->
                  {Dest, C, [Node|R]}
          end, {H, [], []}, ListOfMetadatas),
    correct_redundancies_3(ErrorNodes ++ InconsistentNodes, CorrectNodes, Metadata).


%% @doc correct_redundancies_3/4 - last.
%% @private
-spec(correct_redundancies_3(list(), list(), #?METADATA{}) ->
             ok | {error, any()}).
correct_redundancies_3([], _, _) ->
    ok;
correct_redundancies_3(_, [], _) ->
    {error, 'not_fix_inconsistency'};
correct_redundancies_3(InconsistentNodes, [Node|_] = NodeL, Metadata) ->
    RPCKey = rpc:async_call(Node, leo_storage_api, synchronize,
                            [InconsistentNodes, Metadata]),
    Ret = case rpc:nb_yield(RPCKey, ?DEF_REQ_TIMEOUT) of
              {value, ok} ->
                  ok;
              {value, {error, Cause}} ->
                  {error, Cause};
              {value, not_found = Cause} ->
                  {error, Cause};
              {value, {badrpc = Cause, _}} ->
                  {error, Cause};
              timeout = Cause->
                  {error, Cause}
          end,
    correct_redundancies_4(Ret, InconsistentNodes, NodeL, Metadata).

%% @private
correct_redundancies_4(ok,_InconsistentNodes,_NodeL,_Metadata) ->
    ok;
correct_redundancies_4({error, eof},_InconsistentNodes,_NodeL, Metadata) ->
    case (Metadata#?METADATA.dsize == 0) of
        true ->
            Obj = leo_object_storage_transformer:metadata_to_object(<<>>, Metadata),
            case leo_storage_handler_object:put(Obj, 0, leo_mq) of
                {ok,_} ->
                    ok;
                Error ->
                    Error
            end;
        false ->
            ?error("correct_redundancies_4/4",
                   [{metadata, Metadata},
                    {cause, 'broken_object'}]),
            ok
    end;
correct_redundancies_4({error, not_found = Why},_InconsistentNodes, [Node|_Rest], Metadata) ->
    ?warn("correct_redundancies_4/4",
          [{node, Node},
           {metadata, Metadata}, {cause, Why}]),
    ok;
correct_redundancies_4({error, Why}, InconsistentNodes, [Node|Rest], Metadata) ->
    ?debug("correct_redundancies_4/4",
           [{inconsistent_nodes, InconsistentNodes},
            {node, Node}, {metadata, Metadata}, {cause, Why}]),
    correct_redundancies_3(InconsistentNodes, Rest, Metadata).


%% @doc Relocate an object because of executed "rebalance"
%%      NOTE:
%%          If remote-node status is NOT 'running',
%%          this function cannot operate 'copy'.
%% @private
rebalance_1(#rebalance_message{node = Node,
                               vnode_id = VNodeId,
                               addr_id = AddrId,
                               key = Key} = Msg) ->
    case leo_redundant_manager_api:get_member_by_node(Node) of
        {ok, #member{state = ?STATE_RUNNING}} ->
            ok = decrement_counter(?TBL_REBALANCE_COUNTER, VNodeId),

            case leo_redundant_manager_api:get_redundancies_by_key(Key) of
                {ok, #redundancies{nodes = Redundancies}} ->
                    Ret = delete_node_from_redundancies(Redundancies, Node, []),
                    rebalance_2(Ret, Msg);
                _ ->
                    ok = publish(?QUEUE_ID_PER_OBJECT,
                                 AddrId, Key, ?ERR_TYPE_REPLICATE_DATA),
                    {error, ?ERROR_COULD_NOT_GET_REDUNDANCY}
            end;
        {error, Cause} ->
            ?warn("rebalance_1/1",
                  [{node, Node}, {addr_id, AddrId},
                   {key, Key}, {cause, Cause}]),
            ok = publish(?QUEUE_ID_PER_OBJECT,
                         AddrId, Key, ?ERR_TYPE_REPLICATE_DATA),
            {error, inactive}
    end.

%% @private
rebalance_2({ok,[]},_) ->
    ok;
rebalance_2({ok, Redundancies}, #rebalance_message{node = Node,
                                                   addr_id = AddrId,
                                                   key = Key}) ->
    Redundancies_1 = get_redundancies_with_replicas(
                       AddrId, Key, Redundancies),
    case find_node_from_redundancies(Redundancies_1, erlang:node()) of
        true ->
            case lists:filter(
                   fun(#redundant_node{node = RedundantNode}) ->
                           Node == RedundantNode
                   end, Redundancies_1) of
                [] ->
                    ?MODULE:publish(?QUEUE_ID_PER_OBJECT,
                                    AddrId, Key, ?ERR_TYPE_RECOVER_DATA);
                _ ->
                    send_object_to_remote_node(Node, AddrId, Key)
            end;
        false ->
            ?warn("rebalance_2/2",
                  [{node, Node}, {addr_id, AddrId},
                   {key, Key}, {cause, 'node_not_found'}]),
            publish(?QUEUE_ID_PER_OBJECT,
                    AddrId, Key, ?ERR_TYPE_REPLICATE_DATA)
    end.


%% @doc Retrieve redundancies with a number of replicas
%% @private
-spec(get_redundancies_with_replicas(AddrId, Key, Redundancies) ->
             Redundancies when AddrId::non_neg_integer(),
                               Key::binary(),
                               Redundancies::[#redundancies{}]).
get_redundancies_with_replicas(AddrId, Key, Redundancies) ->
    %% Retrieve redundancies with a number of replicas
    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            case binary_to_term(MetaBin) of
                #?METADATA{num_of_replicas = 0} ->
                    Redundancies;
                #?METADATA{num_of_replicas = NumOfReplicas} ->
                    lists:sublist(Redundancies, NumOfReplicas)
            end;
        _ ->
            Redundancies
    end.


%% @doc Notify a rebalance-progress messages to manager.
%%      - Retrieve # of published messages for rebalance,
%%        after notify a message to manager.
%%
-spec(notify_rebalance_message_to_manager(integer()) ->
             ok | {error, any()}).
notify_rebalance_message_to_manager(VNodeId) ->
    case ets_lookup(?TBL_REBALANCE_COUNTER, VNodeId) of
        {ok, NumOfMessages} ->
            Fun = fun(_Manager, true) ->
                          void;
                     (Manager0, false = Res) ->
                          Manager1 = case is_atom(Manager0) of
                                         true  -> Manager0;
                                         false -> list_to_atom(Manager0)
                                     end,
                          case catch rpc:call(Manager1, leo_manager_api, notify,
                                              [rebalance, VNodeId,
                                               erlang:node(), NumOfMessages],
                                              ?DEF_REQ_TIMEOUT) of
                              ok ->
                                  true;
                              {_, Cause} ->
                                  ?error("notify_rebalance_message_to_manager/1",
                                         [{manager, Manager1},
                                          {vnode_id, VNodeId}, {cause, Cause}]),
                                  Res;
                              timeout ->
                                  Res
                          end
                  end,
            lists:foldl(Fun, false, ?env_manager_nodes(leo_storage)),
            ok;
        Error ->
            Error
    end.


%% @doc Fix consistency of an object between a local-cluster and remote-cluster(s)
%% @private
-spec(fix_consistency_between_clusters(InconsistentData) ->
             ok | {error, Cause} when InconsistentData::#inconsistent_data_with_dc{},
                                      Cause::any()).
fix_consistency_between_clusters(#inconsistent_data_with_dc{
                                    addr_id = AddrId,
                                    key = Key,
                                    del = ?DEL_FALSE}) ->
    case leo_storage_handler_object:get(AddrId, Key, -1) of
        {ok, Metadata, Bin} ->
            Object = leo_object_storage_transformer:metadata_to_object(Metadata),
            leo_sync_remote_cluster:defer_stack(Object#?OBJECT{data = Bin});
        _ ->
            ok
    end;
fix_consistency_between_clusters(#inconsistent_data_with_dc{
                                    cluster_id = ClusterId,
                                    addr_id = AddrId,
                                    key = Key,
                                    del = ?DEL_TRUE}) ->
    Metadata = #?METADATA{cluster_id = ClusterId,
                          addr_id = AddrId,
                          key = Key,
                          dsize = 0,
                          del = ?DEL_TRUE},
    Object = leo_object_storage_transformer:metadata_to_object(Metadata),
    leo_sync_remote_cluster:stack(Object#?OBJECT{data = <<>>}).


%% @doc Remove objects under the directory/bucket
-spec(remove_objects_under_dir(MessageBin) ->
             ok | {error, Cause} when MessageBin::binary(),
                                      Cause::any()).
remove_objects_under_dir(MessageBin) ->
    case catch binary_to_term(MessageBin) of
        {'EXIT',_Cause} ->
            ?warn("remove_objects_under_dir/1",
                  [{qid, ?QUEUE_ID_DEL_DIR},
                   {cause, invalid_data_format}]),
            ok;
        AsyncDeletionMsg ->
            case ?transform_async_deletion_message(AsyncDeletionMsg) of
                {ok, #?MSG_ASYNC_DELETION{addr_id = AddrId,
                                          key = Key,
                                          meta = Metadata}} ->
                    case compare_between_metadatas(Metadata, true) of
                        true ->
                            case catch leo_storage_handler_object:delete(
                                         #?OBJECT{addr_id = AddrId,
                                                  key = Key,
                                                  clock = leo_date:clock(),
                                                  timestamp = leo_date:now(),
                                                  del = ?DEL_TRUE}, 0, false, leo_mq) of
                                ok ->
                                    ok;
                                {error, not_found} ->
                                    ok;
                                {_, Cause} ->
                                    {error, Cause}
                            end;
                        false ->
                            ok;
                        {error, Cause} ->
                            {error, Cause}
                    end;
                _ ->
                    ?warn("remove_objects_under_dir/1",
                          [{qid, ?QUEUE_ID_DEL_DIR},
                           {cause, invalid_data_format}]),
                    ok
            end
    end.


%% @doc Compare MQ's metadata with its latest metadata
-spec(compare_between_metadatas(Metadata, IsDelete) ->
             Result | {error, Cause} when Metadata::#?METADATA{},
                                          IsDelete::boolean(),
                                          Result::boolean(),
                                          Cause::any()).
compare_between_metadatas(undefined, _) ->
    true;
compare_between_metadatas(#?METADATA{addr_id = AddrId,
                                     key = Key,
                                     clock = MsgClock}, IsDelete) ->
    case leo_storage_handler_object:head(AddrId, Key, false) of
        {ok, #?METADATA{clock = Clock}} when MsgClock >= Clock ->
            true;
        {ok, _} ->
            false;
        not_found when IsDelete =:= true ->
            %% for remove_objects_under_dir
            %% return false as the object already deleted on this node
            false;
        not_found ->
            %% for consume with QUEUE_ID_PER_OBJECT
            %% return true as the object to be recovered on this node
            true;
        {error, Cause} ->
            ?error("compare_between_metadatas/2",
                   [{addr_id, AddrId}, {key, Key},
                    {cause, Cause}]),
            {error, ?ERROR_COULD_NOT_GET_META}
    end.


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS-2 ETS related functions
%%--------------------------------------------------------------------
%% @doc Lookup rebalance counter
%% @private
-spec(ets_lookup(Table, Key) ->
             {ok, Value} | {error, Cause} when Table::atom(),
                                               Key::binary(),
                                               Value::integer(),
                                               Cause::any()).
ets_lookup(Table, Key) ->
    case catch ets:lookup(Table, Key) of
        [] ->
            {ok, 0};
        [{_Key, Value}] ->
            {ok, Value};
        {'EXIT', Cause} ->
            {error, Cause}
    end.


%% @doc Increment rebalance counter
%% @private
-spec(increment_counter(Table, Key) ->
             ok when Table::atom(),
                     Key::binary()).
increment_counter(Table, Key) ->
    catch ets:update_counter(Table, Key, 1),
    ok.

%% @doc Decrement rebalance counter
%% @private
-spec(decrement_counter(Table, Key) ->
             ok when Table::atom(),
                     Key::binary()).
decrement_counter(Table, Key) ->
    catch ets:update_counter(Table, Key, -1),
    ok.
