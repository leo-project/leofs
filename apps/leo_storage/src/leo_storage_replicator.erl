%%======================================================================
%%
%% LeoStorage
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
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
%% LeoFS - Replicator.
%% @doc
%% @end
%%======================================================================
-module(leo_storage_replicator).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([replicate/5, init_loop/5]).

-type(error_msg_type() :: ?ERR_TYPE_REPLICATE_DATA  |
                          ?ERR_TYPE_DELETE_DATA).

-record(req_params, {
          pid     :: pid(),
          addr_id :: non_neg_integer(),
          key     :: binary(),
          object  :: #?OBJECT{},
          req_id  :: integer()}).

-record(state, {
          method       :: atom(),
          addr_id      :: non_neg_integer(),
          key          :: binary(),
          object       :: #?OBJECT{},
          num_of_nodes :: pos_integer(),
          req_id       :: non_neg_integer(),
          callback     :: function(),
          errors = []  :: list(),
          is_reply = false ::boolean()
         }).

-define(LEO_STORAGE_REPLICATOR_MSG_TAG, 'leo_storage_replicator').

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Replicate an object to local-node and remote-nodes.
%%
-spec(replicate(Method, Quorum, Nodes, Object, Callback) ->
             any() when Method::put|delete,
                        Quorum::pos_integer(),
                        Nodes::list(),
                        Object:: #?OBJECT{},
                        Callback::function()).
replicate(Method, Quorum, Nodes, Object, Callback) ->
    AddrId = Object#?OBJECT.addr_id,
    Key = Object#?OBJECT.key,
    ReqId = Object#?OBJECT.req_id,
    NumOfNodes = erlang:length(Nodes),

    Pid = self(),
    Ref = make_ref(),
    State = #state{method = Method,
                   addr_id = AddrId,
                   key = Key,
                   object = Object,
                   num_of_nodes = NumOfNodes,
                   req_id = ReqId,
                   callback = Callback,
                   errors = [],
                   is_reply = false},
    MaxProcs = ?env_max_num_of_procs(),
    case (MaxProcs <
              erlang:system_info(process_count)) of
        true ->
            Callback({error, unavailable});
        false ->
            case proc_lib:start_link(?MODULE, init_loop, [NumOfNodes, Quorum, Ref, Pid, State]) of
                {ok, Ref, SubParent} ->
                    replicate_1(Nodes, Ref, SubParent, State);
                _ ->
                    Callback({error, ["Failed to initialize"]})
            end
    end.

init_loop(NumOfNodes, Quorum, Ref, Parent, State) ->
    ok = proc_lib:init_ack(Parent, {ok, Ref, self()}),
    loop(NumOfNodes, Quorum, [], Ref, Parent, State).

%% @private
replicate_1([], Ref,_From, #state{method = Method,
                                  key = Key,
                                  object = Object,
                                  callback = Callback}) ->
    receive
        %% Receive only messages sent from SubParent (_From in this context)
        {?LEO_STORAGE_REPLICATOR_MSG_TAG, Ref, Reply} ->
            Callback(Reply)
    after
        (?DEF_REQ_TIMEOUT + timer:seconds(1)) ->
            %% for recovering message of the repair-obj's MQ
            Metadata = leo_object_storage_transformer:object_to_metadata(Object),
            enqueue(Method, ?ERR_TYPE_REPLICATE_DATA, Metadata),

            %% for watchdog
            ok = leo_storage_msg_collector:notify(?ERROR_MSG_TIMEOUT, Method, Key),
            %% reply error
            Cause = timeout,
            ?warn("replicate/5",
                  [{method, Method},
                   {key, Key}, {cause, Cause}]),
            Callback({error, [Cause]})
    end;
%% for local-node
replicate_1([#redundant_node{node = Node,
                             available = true}|Rest],
            Ref, From, #state{addr_id = AddrId,
                              key = Key,
                              object = Object,
                              req_id = ReqId} = State) when Node == erlang:node() ->
    spawn(fun() ->
                  replicate_fun(Ref, #req_params{pid     = From,
                                                 addr_id = AddrId,
                                                 key     = Key,
                                                 object  = Object,
                                                 req_id  = ReqId})
          end),
    replicate_1(Rest, Ref, From, State);

%% for remote-node
replicate_1([#redundant_node{node = Node,
                             available = true}|Rest], Ref, From, #state{object = Object,
                                                                        req_id = ReqId} = State) ->
    true = rpc:cast(Node, leo_storage_handler_object, put, [Ref, From, Object, ReqId]),
    replicate_1(Rest, Ref, From, State);

%% for unavailable node
replicate_1([#redundant_node{node = Node,
                             available = false}|Rest], Ref, From, State) ->
    %% This message is sent to SubParent
    erlang:send(From, {Ref, {error, {Node, nodedown}}}),
    replicate_1(Rest, Ref, From, State).


%% @doc Waiting for messages (replication)
%% @private
loop(0, 0,_ResL,_Ref,_From, #state{is_reply = true}) ->
    ok;
loop(0, 0, ResL, Ref, From, #state{method = Method}) ->
    %% This message is sent to Parent so need to add a tag
    erlang:send(From, {?LEO_STORAGE_REPLICATOR_MSG_TAG, Ref, {ok, Method, hd(ResL)}});
loop(_, W,_ResL, Ref, From, #state{num_of_nodes = N,
                                   errors = E}) when (N - W) < length(E) ->
    %% This message is sent to Parent so need to add a tag
    erlang:send(From, {?LEO_STORAGE_REPLICATOR_MSG_TAG, Ref, {error, E}});
loop(N, W, ResL, Ref, From, #state{method = Method,
                                   key = Key,
                                   object = Object,
                                   errors = E,
                                   callback = Callback,
                                   is_reply = IsReply} = State) ->
    receive
        {Ref, {ok, Checksum}} ->
            ResL_1 = [Checksum|ResL],
            {W_1, State_1} =
                case ((W - 1) < 1) of
                    true when IsReply == false ->
                        %% This message is sent to Parent so need to add a tag
                        erlang:send(From, {?LEO_STORAGE_REPLICATOR_MSG_TAG, Ref, {ok, Method, hd(ResL_1)}}),
                        {0, State#state{is_reply = true}};
                    true ->
                        {0, State};
                    false ->
                        {W - 1, State}
                end,
            loop(N-1, W_1, ResL_1, Ref, From, State_1);
        {Ref, {error, {_Node, not_found}}} when Method == 'delete'->
            {W_1, State_1} =
                case ((W - 1) < 1) of
                    true when IsReply == false ->
                        %% This message is sent to Parent so need to add a tag
                        erlang:send(From, {?LEO_STORAGE_REPLICATOR_MSG_TAG, Ref, {ok, Method, 0}}),
                        {0, State#state{is_reply = true}};
                    true ->
                        {0, State};
                    false ->
                        {W - 1, State}
                end,
            loop(N-1, W_1, [0|ResL], Ref, From, State_1);
        {Ref, {error, {Node, Cause}}} ->
            Metadata = leo_object_storage_transformer:object_to_metadata(Object),
            enqueue(Method, ?ERR_TYPE_REPLICATE_DATA, Metadata),

            State_1 = State#state{errors = [{Node, Cause}|E]},
            loop(N-1, W, ResL, Ref, From, State_1)
    after
        ?DEF_REQ_TIMEOUT ->
            case (W > 0) of
                true ->
                    %% for recovering message of the repair-obj's MQ
                    Metadata = leo_object_storage_transformer:object_to_metadata(Object),
                    enqueue(Method, ?ERR_TYPE_REPLICATE_DATA, Metadata),

                    %% set reply
                    Cause = timeout,
                    ?warn("loop/6",
                          [{method, Method}, {key, Key}, {cause, Cause}]),
                    Callback({error, [Cause]});
                false ->
                    void
            end
    end.


%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Request a replication-message for local-node.
%%
-spec(replicate_fun(reference(), #req_params{}) ->
             {ok, atom()} | {error, atom(), any()}).
replicate_fun(Ref, #req_params{pid     = Pid,
                               key     = Key,
                               object  = Object,
                               req_id  = ReqId}) ->
    %% Ref  = make_ref(),
    Ret  = case leo_storage_handler_object:put({Object, Ref}) of
               {ok, Ref, Checksum} ->
                   {Ref, {ok, Checksum}};
               {error, Ref, not_found = Cause} ->
                   {Ref, {error, {node(), Cause}}};
               {error, Ref, Cause} ->
                   ?warn("replicate_fun/2",
                         [{key, Key}, {node, local},
                          {req_id, ReqId}, {cause, Cause}]),
                   {Ref, {error, {node(), Cause}}}
           end,
    %% This message is sent to SubParent
    erlang:send(Pid, Ret).


%% @doc Input a message into the queue.
-spec(enqueue(Method, Type, Metadata) ->
             ok when Method::type_of_method(),
                     Type::error_msg_type(),
                     Metadata::#?METADATA{}).
enqueue('put', ?ERR_TYPE_REPLICATE_DATA = Type, Metadata) ->
    QId = ?QUEUE_ID_PER_OBJECT,
    case leo_storage_mq:publish(QId, Metadata, Type) of
        ok ->
            ok;
        {error, Cause} ->
            #?METADATA{addr_id = AddrId,
                       key = Key} = Metadata,
            ?warn("enqueue/1",
                  [{qid, QId}, {addr_id, AddrId},
                   {key, Key}, {type, Type}, {cause, Cause}])
    end;
enqueue('delete', _Type, Metadata) ->
    QId = ?QUEUE_ID_ASYNC_DELETION,
    case leo_storage_mq:publish(QId, Metadata) of
        ok ->
            ok;
        {error, Cause} ->
            #?METADATA{addr_id = AddrId,
                       key = Key} = Metadata,
            ?warn("enqueue/1",
                  [{qid, QId}, {addr_id, AddrId},
                   {key, Key}, {cause, Cause}])
    end.
