%%======================================================================
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
%% ---------------------------------------------------------------------
%% Leo Storage - Deletion Directory Handler
%%
%% @doc Leo Storage's Deletion Bucket Handler
%% @reference https://github.com/leo-project/leo_storage/blob/master/src/leo_storage_handler_del_bucket.erl
%% @end
%%======================================================================
-module(leo_storage_handler_del_directory).

-behaviour(gen_server).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").


%% API
-export([start_link/0, stop/0]).
-export([enqueue/1, enqueue/2, enqueue/3,
         get_cached_items/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-undef(TIMEOUT).
-define(TIMEOUT, timer:seconds(30)).

-undef(INTERVAL).
-define(INTERVAL, timer:seconds(3)).

%% Timeout for communicating with leo_manager
%% This value should be as small as possible to solve https://github.com/leo-project/leofs/issues/892
-define(TIMEOUT_FOR_RPC, timer:seconds(5)).

-record(state, {
          num_of_workers = 1 :: pos_integer(),
          manager_nodes = [] :: [node()],
          cached_items = [] :: [binary()]
         }).


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
%%
-spec(start_link() ->
             {ok, pid()} | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Stop this server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop, ?TIMEOUT).


%% @doc Insert a record of del-bucket
-spec(enqueue(Directory) ->
             ok | {error, any()} when Directory::binary()).
enqueue(Directory) ->
    enqueue(?TYPE_DEL_BUCKET, Directory).

-spec(enqueue(Type, Directory) ->
             ok | {error, any()} when Type::del_dir_type(),
                                      Directory::binary()).
enqueue(Type, Directory) ->
    enqueue(Type, Directory, false).

-spec(enqueue(Type, Directory, IsClient) ->
             ok | {error, any()} when Type::del_dir_type(),
                                      Directory::binary(),
                                      IsClient::boolean()).
enqueue(Type, Directory, IsClient) ->
    gen_server:call(?MODULE, {enqueue, Type, Directory, IsClient}, ?TIMEOUT).


%% @doc Retrieve cached items which contains bucket-names
-spec(get_cached_items() ->
             {ok, Items} when BucketName::binary(),
                              Items::[BucketName]).
get_cached_items() ->
    gen_server:call(?MODULE, get_cached_items, ?TIMEOUT).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
%% @doc Initiates the server
init([]) ->
    erlang:send_after(?INTERVAL, self(), trigger),
    State = get_current_state(#state{num_of_workers = ?env_del_dir_workers(),
                                     manager_nodes = ?env_manager_nodes()}),
    {ok, State}.

%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop,_From, State) ->
    {stop, shutdown, ok, State};

handle_call({enqueue, Type, Directory, IsClient},_From, #state{cached_items = CachedItems} = State) ->
    %% Check a registered record to decline the same one
    {Ret, NewState} =
        case leo_backend_db_api:get(
               ?DEL_DIR_STATE_DB_ID, Directory) of
            not_found ->
                case update_state(#del_dir_state{
                                     mq_id = null,
                                     directory = Directory,
                                     type = Type,
                                     state = ?STATE_PENDING,
                                     enqueued_at = leo_date:clock(),
                                     timestamp = leo_date:now()
                                    }) of
                    ok when IsClient == false->
                        case run(Type, ?STATE_PENDING, null, Directory, State) of
                            {ok, State_1} ->
                                ?info("run/5", [{"msg: enqueued", Directory}]),
                                {ok, State_1#state{cached_items = [Directory|CachedItems]}};
                            {error,_} ->
                                {error, State}
                        end;
                    ok ->
                        ?info("handle_call/3 - enqueue",  [{"msg: enqueued", Directory}]),
                        {ok, State};
                    {error,_Cause} ->
                        {{error, ?ERROR_ENQUEUE_FAILURE}, State}
                end;
            {ok,_} ->
                {ok, State};
            {_, Cause} ->
                ?error("handle_call/3 - enqueue", [{cause, Cause}]),
                {{error, ?ERROR_ENQUEUE_FAILURE}, State}
        end,
    {reply, Ret, NewState};

handle_call(get_cached_items,_From, #state{cached_items = CachedItems} = State) ->
    {reply, {ok, CachedItems}, State};

handle_call(_Msg,_From, State) ->
    {reply, ok, State}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(trigger, State) ->
    _ = check_stats(self(), State),
    {_, State_1} = dequeue(State),

    %% Retrieve the latest items
    State_2 = get_current_state(State_1),
    erlang:send_after(?INTERVAL, self(), trigger),
    {noreply, State_2};

handle_info({next, MQId, Directory, 0}, State) ->
    %% Finished
    case leo_backend_db_api:get(?DEL_DIR_STATE_DB_ID, Directory) of
        {ok, DelDirStateBin} ->
            case catch binary_to_term(DelDirStateBin) of
                #del_dir_state{mq_id = MQId,
                               type = Type} ->
                    run(Type, ?STATE_FINISHED, MQId, Directory, State);
                {'EXIT', Cause} ->
                    ?error("handle_info/2 - next", [{cause, Cause}])
            end;
        not_found ->
            ok;
        {error, Cause} ->
            ?error("handle_info/2 - next", [{cause, Cause}])
    end,
    {noreply, State};

handle_info({next,_MQId,_Directory,_TotalMsgs}, State) ->
    {noreply, State};

handle_info({failed, MQId, Type, Directory}, State) ->
    update_state(#del_dir_state{
                    mq_id = MQId,
                    directory = Directory,
                    type = Type,
                    state = ?STATE_PENDING,
                    timestamp = leo_date:now()}),
    {noreply, State};

handle_info({enqueued, MQId, Type, Directory}, State) ->
    {ok, State_1} = run(Type, ?STATE_MONITORING, MQId, Directory, State),
    {noreply, State_1};

handle_info(_Info, State) ->
    {noreply, State}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason,_State) ->
    ?info("terminate/2", [{"terminate with reason", _Reason}]),
    rollback_to_pending(),
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Private Functions
%%====================================================================
%% @doc Retrieve a candidate del-bucket message whose current state is 'PENDING',
%%      then assign it to one del-buclet-queue  in case of existing unoccupied workers
%% @private
-spec(dequeue(State) ->
             {ok, State} when State::#state{}).
dequeue(#state{num_of_workers = NumOfWorkers} = State) ->
    case get_ongoing_workers(NumOfWorkers) of
        {ok, DelBucketStateList} ->
            List = lists:sort(
                     lists:subtract(
                       ?del_dir_id_list(),
                       lists:foldl(
                         fun({_, DelBucketStateBin}, Acc) ->
                                 case catch binary_to_term(DelBucketStateBin) of
                                     #del_dir_state{mq_id = MQId,
                                                    state = ProcState}
                                       when ProcState == ?STATE_ENQUEUING;
                                            ProcState == ?STATE_MONITORING;
                                            ProcState == ?STATE_FINISHED ->
                                         [MQId|Acc];
                                     _ ->
                                         Acc
                                 end
                         end, [], DelBucketStateList))),
            case List of
                [H|_] ->
                    Ret = dequeue_1(State, DelBucketStateList, H),
                    {Ret, State};
                [] ->
                    {ok, State}
            end;
        not_found ->
            {ok, State};
        {_, Cause} ->
            ?error("dequeue/1", [{cause, Cause}]),
            {{error, Cause}, State}
    end.

%% @private
-spec(dequeue_1(State, DelBucketStateList, MQId) ->
             ok |
             not_found |
             {error, Cause} when State::#state{},
                                 DelBucketStateList::[],
                                 MQId::mq_id(),
                                 Cause::any()).
dequeue_1(_,[],_) ->
    not_found;
dequeue_1(State, [{_, DelBucketStateBin}|DelBucketStateList], MQId) ->
    case catch binary_to_term(DelBucketStateBin) of
        #del_dir_state{type = Type,
                       directory = Directory,
                       state = ?STATE_PENDING,
                       enqueued_at = EnqueuedAt} ->
            From = self(),
            _Pid = spawn(
                     fun() ->
                             insert_messages(From, MQId, Type, Directory, EnqueuedAt)
                     end),
            % receive 'enqueuing' message here to change the state from STATE_PENDING to STATE_ENQUEUING
            % for preventing insert_messages from being invoked multiple times.
            receive
                {enqueuing, MQId, Type, Directory} ->
                    run(Type, ?STATE_ENQUEUING, MQId, Directory, State)
            end,
            ok;
        #del_dir_state{} ->
            dequeue_1(State, DelBucketStateList, MQId);
        {_, Cause} ->
            ?error("dequeue_1/3", [{cause, Cause}]),
            {error, Cause}
    end.


%% @private
-spec(insert_messages(From, MQId, Type, Directory, EnqueuedAt) ->
             ok | {error, Cause} when From::pid(),
                                      MQId::mq_id(),
                                      Type::del_dir_type(),
                                      Directory::binary(),
                                      EnqueuedAt::pos_integer(),
                                      Cause::any()).
insert_messages(From, MQId, Type, Directory, EnqueuedAt) ->
    erlang:send(From, {enqueuing, MQId, Type, Directory}),

    DirTrailingSlash = case binary:last(Directory) of
        $/ ->
            Directory;
        _ ->
            << Directory/binary, "/" >>
    end,

    case leo_storage_handler_object:prefix_search_and_remove_objects(
           MQId, DirTrailingSlash, EnqueuedAt) of
        {ok,_TotalMsgs} ->
            erlang:send(From, {enqueued, MQId, Type, Directory});
        {error, Cause} ->
            ?error("insert_messages/3", [{cause, Cause}]),
            erlang:send(From, {failed, MQId, Type, Directory})
    end.


%% @doc Retrieve the records whose state is pending/enqueuing/monitoring/finished.
%%      STATE_FINISHED should be returned as its state means notifying to managers still doesn't succeed.
%% @private
get_ongoing_workers(NumOfWorkers) ->
    %% Retrieve ongoing workers
    Cond = fun(_K, V) ->
                   case catch binary_to_term(V) of
                       #del_dir_state{state = ProcState}
                         when ProcState == ?STATE_PENDING;
                              ProcState == ?STATE_ENQUEUING;
                              ProcState == ?STATE_MONITORING;
                              ProcState == ?STATE_FINISHED ->
                           true;
                       _ ->
                           false
                   end
           end,
    case leo_backend_db_api:first_n(?DEL_DIR_STATE_DB_ID,
                                    NumOfWorkers, Cond) of
        {ok, DelBucketStateList} ->
            {ok, DelBucketStateList};
        not_found ->
            {ok, []};
        {_, Cause} ->
            ?error("get_ongoing_workers/1", [{cause, Cause}]),
            {error, Cause}
    end.

%% @doc Rollback the records whose state is enqueuing into pending
%%      When leo_storage goes down in the middle of enqueuing
%%      in order to retry the enqueuing at the next restart.
%% @private
rollback_to_pending() ->
    %% Retrieve ALL enqueuing records
    Cond = fun(_K, V) ->
                   case catch binary_to_term(V) of
                       #del_dir_state{state = ProcState}
                         when ProcState == ?STATE_ENQUEUING ->
                           true;
                       _ ->
                           false
                   end
           end,
    case leo_backend_db_api:first_n(?DEL_DIR_STATE_DB_ID,
                                    round(math:pow(2, 32)), Cond) of
        {ok, DelBucketStateList} ->
            rollback_to_pending(DelBucketStateList);
        not_found ->
            ok;
        {_, Cause} ->
            ?error("rollback_to_pending/0", [{cause, Cause}])
    end.

rollback_to_pending([]) ->
    ok;
rollback_to_pending([{_, DelBucketStateBin} | DelBucketStateList]) ->
    case catch binary_to_term(DelBucketStateBin) of
        #del_dir_state{} = DelBucketState ->
            rollback_to_pending_1(DelBucketState);
        _ ->
            void
    end,
    rollback_to_pending(DelBucketStateList);
rollback_to_pending([_|DelBucketStateList]) ->
    rollback_to_pending(DelBucketStateList).

rollback_to_pending_1(#del_dir_state{mq_id = MQId,
                                     type = Type,
                                     directory = Directory}) ->
    %% rollback to pending
    update_state(#del_dir_state{
                    mq_id = MQId,
                    directory = Directory,
                    type = Type,
                    state = ?STATE_PENDING,
                    timestamp = leo_date:now()}).

%% @doc FSM
%% @private
-spec(check_stats(From, State) ->
             ok when From::pid(),
                     State::#state{}).
check_stats(From, #state{num_of_workers = NumOfWorkers} = State) ->
    %% Retrieve not pending requests upto 'num_of_workers'
    case get_ongoing_workers(NumOfWorkers) of
        {ok, DelBucketStateList} ->
            check_stats_1(DelBucketStateList, From, State);
        Error ->
            Error
    end.


%% @private
-spec(check_stats_1([#del_dir_state{}], From, State) ->
             ok when From::pid(),
                     State::#state{}).
check_stats_1([],_,_) ->
    ok;
check_stats_1([{_, DelBucketStateBin} | DelBucketStateList], From, State) ->
    case catch binary_to_term(DelBucketStateBin) of
        #del_dir_state{} = DelBucketState ->
            check_stats_2(DelBucketState, From, State);
        _ ->
            void
    end,
    check_stats_1(DelBucketStateList, From, State);
check_stats_1([_|DelBucketStateList], From, State) ->
    check_stats_1(DelBucketStateList, From, State).

%% @private
check_stats_2(#del_dir_state{mq_id = MQId,
                             type = Type,
                             directory = Directory,
                             state = ?STATE_PENDING,
                             enqueued_at = EnqueuedAt}, From,_State) when MQId /= null,
                                                                          Type == ?TYPE_DEL_DIR ->
    case leo_mq_api:count(MQId) of
        {ok, 0} ->
            _Pid = spawn(
                     fun() ->
                             insert_messages(From, MQId, Type, Directory, EnqueuedAt)
                     end),
            ok;
        {ok,_Count} ->
            update_state(#del_dir_state{
                            mq_id = MQId,
                            directory = Directory,
                            type = Type,
                            state = ?STATE_MONITORING,
                            timestamp = leo_date:now()});
        _ ->
            ok
    end;
check_stats_2(#del_dir_state{mq_id = MQId,
                             type = Type,
                             directory = Directory,
                             state = ?STATE_ENQUEUING,
                             is_notification_successful = false}, _From, State) ->
    % ONLY call when the notification still doesn't succeed
    run(Type, ?STATE_ENQUEUING, MQId, Directory, State);
check_stats_2(#del_dir_state{mq_id = MQId,
                             type = Type,
                             directory = Directory,
                             state = ?STATE_MONITORING,
                             is_notification_successful = NotificationFlag}, From, State) ->
    case leo_mq_api:count(MQId) of
        {ok, 0} ->
            erlang:send(From, {next, MQId, Directory, 0}),
            ok;
        {ok,_Count} when NotificationFlag == false ->
            run(Type, ?STATE_MONITORING, MQId, Directory, State);
        {ok,_Count} ->
            ok;
        _Other ->
            ok
    end;
check_stats_2(#del_dir_state{mq_id = MQId,
                             directory = Directory,
                             state = ?STATE_FINISHED}, From,_State) ->
    erlang:send(From, {next, MQId, Directory, 0}),
    ok;
check_stats_2(_,_,_) ->
    ok.


%% @private
-spec(update_state(DelDirState) ->
             ok | {error, Cause} when DelDirState::#del_dir_state{},
                                      Cause::any()).
update_state(#del_dir_state{directory = Directory} = DelDirState) ->
    case leo_backend_db_api:put(?DEL_DIR_STATE_DB_ID, Directory,
                                term_to_binary(DelDirState)) of
        ok ->
            ok;
        {error, Cause} ->
            ?error("update_state/1", [{cause, Cause}]),
            {error, Cause}
    end.


%% @private
-spec(get_current_state(State) ->
             State when State::#state{}).
get_current_state(State) ->
    %% Retrieve the latest items
    case catch leo_backend_db_api:fetch(
           ?DEL_DIR_STATE_DB_ID, <<>>,
           fun(K,_V, Acc) ->
                   [K| Acc]
           end) of
        {ok, Dirs} ->
            State#state{cached_items = Dirs};
        not_found ->
            State#state{cached_items = []};
        {_, Cause} ->
            ?error("get_current_state/1", [{cause, Cause}]),
            State
    end.


%% @doc A callback function
%% @private
-spec(run(Type, State, MQId, Directory, State) ->
             {ok, State} when Type::del_dir_type(),
                              State::del_dir_state(),
                              MQId::atom(),
                              Directory::binary(),
                              State::#state{}).
%% ::: DELETION-BUCKET :::
run(?TYPE_DEL_BUCKET, ProcState,_MQId,_Directory, State) when ProcState == ?STATE_PENDING ->
    {ok, State};

run(?TYPE_DEL_BUCKET = Type, ProcState, MQId, Directory,
    #state{manager_nodes = ManagerNodes} = State) when ProcState == ?STATE_ENQUEUING ->
    case update_state(#del_dir_state{
                         mq_id = MQId,
                         directory = Directory,
                         type = Type,
                         state = ProcState,
                         is_notification_successful = false,
                         timestamp = leo_date:now()}) of
        ok ->
            void;
        {error, Cause} ->
            ?error("run/5 - ENQUEUING ", [{cause, Cause}])
    end,
    notify_current_state_to_manager(
        ManagerNodes, Directory, ?del_dir_state_to_atom(ProcState)),
    case update_state(#del_dir_state{
                         mq_id = MQId,
                         directory = Directory,
                         type = Type,
                         state = ProcState,
                         is_notification_successful = true,
                         timestamp = leo_date:now()}) of
        ok ->
            void;
        {error, Cause2} ->
            ?error("run/5 - ENQUEUING", [{cause, Cause2}])
    end,
    {ok, State};

run(?TYPE_DEL_BUCKET = Type, ProcState, MQId, Directory,
    #state{manager_nodes = ManagerNodes} = State) when ProcState == ?STATE_MONITORING ->
    case update_state(#del_dir_state{
                         mq_id = MQId,
                         directory = Directory,
                         type = Type,
                         state = ProcState,
                         is_notification_successful = false,
                         timestamp = leo_date:now()}) of
        ok ->
            void;
        {error, Cause} ->
            ?error("run/5 - MONITORING", [{cause, Cause}])
    end,
    notify_current_state_to_manager(
        ManagerNodes, Directory, ?del_dir_state_to_atom(ProcState)),
    case update_state(#del_dir_state{
                         mq_id = MQId,
                         directory = Directory,
                         type = Type,
                         state = ProcState,
                         is_notification_successful = true,
                         timestamp = leo_date:now()}) of
        ok ->
            void;
        {error, Cause2} ->
            ?error("run/5 - MONITORING", [{cause, Cause2}])
    end,
    {ok, State};

run(?TYPE_DEL_BUCKET = Type, ?STATE_FINISHED = ProcState, MQId, Directory,
    #state{manager_nodes = ManagerNodes} = State) ->
    Node = node(),
    ProcState_1 = ?del_dir_state_to_atom(ProcState),

    %% Update the state to STATE_FINISHED in order to retry notify_del_dir_state
    %% when the following rpc:call failed.
    case update_state(#del_dir_state{
                         mq_id = MQId,
                         directory = Directory,
                         type = Type,
                         state = ProcState,
                         timestamp = leo_date:now()}) of
        ok ->
            void;
        {error, Cause0} ->
            ?error("run/5", [{cause, Cause0}])
    end,

    case (lists:foldl(
            fun(Manager, false) ->
                    case rpc:call(Manager, leo_manager_api,
                                  notify_del_dir_state,
                                  [Node, Directory, ProcState_1], ?TIMEOUT_FOR_RPC) of
                        ok ->
                            true;
                        not_found ->
                            true;
                        Other ->
                            %% {badrpc, Reason} or {error, Reason} returned
                            ?error("run/5 - FINISHED", [{cause, Other}]),
                            false
                    end;
               (_, true) ->
                    true
            end, false, ManagerNodes)) of
        true ->
            %% After finishing notification of its message to LeoManager,
            %% delete the record of del-bucket state
            case leo_backend_db_api:delete(
                   ?DEL_DIR_STATE_DB_ID, Directory) of
                ok ->
                    ?info("run/5", [{"msg: dequeued and removed (bucket)", Directory}]);
                {error, Cause} ->
                    ?error("run/5", [{cause, Cause}])
            end;
        false ->
            void
    end,
    {ok, State};

%% ::: DELETION-DIRECTORY :::
run(?TYPE_DEL_DIR = Type, ProcState,_MQId, Directory, State) when ProcState == ?STATE_PENDING ->
    %% Retrieve candidates of a del-dir
    case leo_redundant_manager_api:get_members() of
        {ok, Nodes} ->
            Nodes_1 = lists:delete(node(), [N || #member{node = N} <- Nodes]),
            BadList = case Nodes_1 of
                          [] ->
                              [];
                          _ ->
                              {_, BadNodes} = rpc:multicall(
                                                Nodes_1, ?MODULE, enqueue,
                                                [Type, Directory, true], ?DEF_REQ_TIMEOUT),
                              BadNodes
                      end,
            case BadList of
                [] -> void;
                _ ->
                    ?error("run/5 - PENDING", [{cause, "rpc:multicall failed"}, {bad_nodes, BadList}])
            end,
            lists:foreach(
              fun(N) ->
                      case leo_storage_mq:publish(
                             ?QUEUE_ID_REQ_DEL_DIR, N, Directory) of
                          ok ->
                              ok;
                          {error, Cause} ->
                              ?error("run/5", [{cause, Cause}])
                      end
              end, BadList);
        _ ->
            void
    end,
    {ok, State};

run(?TYPE_DEL_DIR = Type, ProcState, MQId, Directory, State) when ProcState == ?STATE_ENQUEUING;
                                                                  ProcState == ?STATE_MONITORING ->
    case leo_mq_api:count(MQId) of
        {ok, 0} ->
            update_state(#del_dir_state{
                            mq_id = MQId,
                            directory = Directory,
                            type = Type,
                            state = ?STATE_FINISHED,
                            timestamp = leo_date:now()});
        {ok,_} ->
            update_state(#del_dir_state{
                            mq_id = MQId,
                            directory = Directory,
                            type = Type,
                            state = ?STATE_MONITORING,
                            timestamp = leo_date:now()});
        _ ->
            void
    end,
    {ok, State};

run(?TYPE_DEL_DIR, ?STATE_FINISHED,_MQId, Directory, State) ->
    case leo_backend_db_api:delete(
           ?DEL_DIR_STATE_DB_ID, Directory) of
        ok ->
            ?info("run/5", [{"msg: dequeued and removed (dir)", Directory}]);
        {error, Cause} ->
            ?error("run/5", [{cause, Cause}])
    end,
    {ok, State};
run(_,_,_,_, State) ->
    {ok, State}.


%% @doc Notify the current state of a MQ to the manager-node
%% @private
notify_current_state_to_manager(ManagerNodes,_Directory,_ProcState) ->
    notify_current_state_to_manager(ManagerNodes, ManagerNodes,_Directory,_ProcState).
notify_current_state_to_manager(ManagerNodes, [],_Directory,_ProcState) ->
    %% retry after 5 seconds
    timer:sleep(timer:seconds(5)),
    notify_current_state_to_manager(ManagerNodes, ManagerNodes,_Directory,_ProcState);
notify_current_state_to_manager(ManagerNodes, [Manager|Acc], Directory, ProcStateAtom) ->
    case rpc:call(Manager, leo_manager_api,
                  notify_del_dir_state,
                  [node(), Directory, ProcStateAtom], ?TIMEOUT_FOR_RPC) of
        ok ->
            true;
        not_found ->
            true;
        Other ->
            %% {badrpc, Reason} or {error, Reason} returned
            ?error("notify_current_state_to_manager/4", [{cause, Other}]),
            notify_current_state_to_manager(ManagerNodes, Acc, Directory, ProcStateAtom)
    end.
