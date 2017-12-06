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
-module(leo_manager_del_bucket_handler).
-behaviour(gen_server).

-include("leo_manager.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, stop/0]).
-export([enqueue/2, notify/3, find_by_bucket_name/1, find_all/0, change_status/3, status/1]).
-export([delete_by_bucket_name/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, timer:seconds(30)).

%% Timeout for communicating with leo_storage
%% This value should be as small as possible to solve https://github.com/leo-project/leofs/issues/892
-define(TIMEOUT_FOR_RPC, timer:seconds(5)).

%%====================================================================
%% API
%%====================================================================
-spec(start_link() ->
             ok | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Stop this server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop, ?DEF_TIMEOUT).


%% @doc Insert a record of del-bucket
-spec(enqueue(AccessKeyBin, BucketName) ->
             ok | {error, any()} when AccessKeyBin::binary(),
                                      BucketName::binary()).
enqueue(AccessKeyBin, BucketName) ->
    gen_server:call(?MODULE, {enqueue, AccessKeyBin, BucketName}, ?DEF_TIMEOUT).


%% @doc Notify a del-bucket message to the node
-spec(notify(NodeType, Node, BucketName) ->
             ok | {error, any()} when NodeType::dest_node_type(),
                                      Node::node(),
                                      BucketName::binary()).
notify(NodeType, Node, BucketName) ->
    gen_server:call(?MODULE, {notify, NodeType, Node, BucketName}, ?DEF_TIMEOUT).


%% @doc Retrieve a del-bucket's state by bucket-name
-spec(find_by_bucket_name(BucketName) ->
             ok | {error, any()} when BucketName::binary()).
find_by_bucket_name(BucketName) ->
    gen_server:call(?MODULE, {find_by_bucket_name, BucketName}, ?DEF_TIMEOUT).


%% @doc Retrieve all del-buckets' state
-spec(find_all() ->
             {ok, [#del_bucket_state{}]} | not_found | {error, any()}).
find_all() ->
    gen_server:call(?MODULE, find_all, ?DEF_TIMEOUT).


%% @doc Update the state of del-bucket processing of  a storage-node
-spec(change_status(Node, BucketName, ProcState) ->
             ok | {error, any()} when Node::node(),
                                      BucketName::binary(),
                                      ProcState::del_bucket_state()).
change_status(Node, BucketName, ProcState) ->
    gen_server:call(?MODULE, {change_status, Node, BucketName, ProcState}, ?DEF_TIMEOUT).


%% @doc Retrieve the state of the del-bucket's processing
-spec(status(BucketName) ->
             ok | {error, any()} when BucketName::binary()).
status(BucketName) ->
    gen_server:call(?MODULE, {status, BucketName}, ?DEF_TIMEOUT).

%% @doc Delete del-bucket's states by bucket-name
-spec(delete_by_bucket_name(BucketName) ->
             ok | {error, any()} when BucketName::binary()).
delete_by_bucket_name(BucketName) ->
    gen_server:call(?MODULE, {delete_by_bucket_name, BucketName}, ?DEF_TIMEOUT).

%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([]) ->
    erlang:send_after(?dequeue_interval(), self(), dequeue),
    {ok, null}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call({enqueue, AccessKeyBin, BucketName}, _From, State) ->
    Ret = case leo_redundant_manager_api:get_members() of
              {ok, Members} ->
                  Now = leo_date:now(),
                  DelBucketStateList =
                      lists:foldl(
                        fun(#member{node = Node,
                                    state = ProcState}, Acc) when ProcState == ?STATE_SUSPEND;
                                                                  ProcState == ?STATE_RUNNING;
                                                                  ProcState == ?STATE_STOP ->
                                ?info("handle_call/3 - enqueue", [{"bucket_name", BucketName},
                                                                  {"node", Node}]),
                                [#del_bucket_state{
                                    id = term_to_binary({BucketName, Node}),
                                    bucket_name = BucketName,
                                    node = Node,
                                    state = ?STATE_PENDING,
                                    timestamp = Now} | Acc];
                           (_, Acc) ->
                                Acc
                        end, [], Members),

                  case leo_manager_mnesia:bulk_insert_del_bucket_info(
                         DelBucketStateList) of
                      ok ->
                          case leo_s3_bucket:delete(AccessKeyBin, BucketName) of
                              ok ->
                                  %% Notify a del-bucket message to gateway-nodes
                                  case leo_manager_api:call_gateway_api(
                                         delete_bucket, [AccessKeyBin, BucketName, undefined]) of
                                      ok ->
                                          ok;
                                      {error, Reason} ->
                                          ?error("handle_call/3, enqueue", [{cause, Reason}]),
                                          {error, ?ERROR_COULD_NOT_ACCESS_GATEWAY}
                                  end;
                              {error, badarg} ->
                                  {error, ?ERROR_INVALID_BUCKET_FORMAT};
                              {error, _Cause} ->
                                  {error, ?ERROR_COULD_NOT_STORE}
                          end;
                      Error ->
                          Error
                  end;
              Error ->
                  Error
          end,
    {reply, Ret, State};

handle_call({notify, NodeType, Node, BucketName}, _From, State) ->
    Ret = notify_fun(NodeType, Node, BucketName),
    {reply, Ret, State};

handle_call({find_by_bucket_name, BucketName},_From, State) ->
    Ret = case leo_manager_mnesia:get_del_bucket_state_by_bucket_name(BucketName) of
              {ok, DelBucketStateList} ->
                  {ok, DelBucketStateList};
              not_found = Cause ->
                  Cause;
              {error, Reason} ->
                  ?error("handle_call/3, find_by_bucket_name", [{cause, Reason}]),
                  {error, ?ERROR_MNESIA_PROC_FAILURE}
          end,
    {reply, Ret, State};


handle_call(find_all,_From, State) ->
    Ret = leo_manager_mnesia:get_del_bucket_state_all(),
    {reply, Ret, State};

handle_call({change_status, Node, BucketName, ProcState},_From, State) ->
    Ret = case leo_manager_mnesia:get_del_bucket_state_by_bucket_name_and_node(
                 BucketName, Node) of
              {ok, DelBucketState} ->
                  leo_manager_mnesia:update_del_bucket_state(
                    DelBucketState#del_bucket_state{state = ProcState,
                                                    timestamp = leo_date:now()});
              not_found = Cause ->
                  Cause;
              {error, Reason} ->
                  ?error("handle_call/3, change_status", [{cause, Reason}]),
                  {error, ?ERROR_MNESIA_PROC_FAILURE}
          end,
    {reply, Ret, State};

handle_call({delete_by_bucket_name, BucketName},_From, State) ->
    Ret = case leo_manager_mnesia:delete_del_bucket_state(BucketName) of
              ok ->
                  ok;
              {error, Reason} ->
                  ?error("handle_call/3, delete_by_bucket_name", [{cause, Reason}]),
                  {error, ?ERROR_MNESIA_PROC_FAILURE}
          end,
    {reply, Ret, State};

handle_call({status,_BucketName}, _From, State) ->
    {reply, ok, State}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(dequeue, State) ->
    %% Dequeue a message to request it to storage-nodes
    case leo_manager_mnesia:get_del_bucket_state_by_state(?STATE_PENDING) of
        {ok, DelBucketStateList_1} ->
            dequeue(DelBucketStateList_1);
        not_found ->
            void;
        {error, Reason} ->
            ?error("handle_info/2, dequeue", [{cause, Reason}])
    end,

    %% Remove messages which are expired
    case leo_manager_mnesia:get_del_bucket_state_all() of
        {ok, DelBucketStateList_2} ->
            after_completion(DelBucketStateList_2);
        not_found ->
            void;
        {error, Cause} ->
            ?error("handle_info/2, dequeue", [{cause, Cause}])
    end,

    erlang:send_after(?dequeue_interval(), self(), dequeue),
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Private Functions
%%====================================================================
%% @private
dequeue([]) ->
    ok;
dequeue([#del_bucket_state{bucket_name = BucketName,
                           node = Node} | Acc]) ->
    _ = notify_fun(?NODE_TYPE_STORAGE, Node, BucketName),
    dequeue(Acc).


%% @private
notify_fun(?NODE_TYPE_STORAGE, Node, BucketName) ->
    case rpc:call(Node, leo_storage_handler_del_directory, enqueue,
                  [BucketName], ?TIMEOUT_FOR_RPC) of
        ok ->
            ?info("notify_fun/3", [{"node", Node},
                                   {"bucket_name", BucketName}]),
            ok;
        Other ->
            case Other of
                {_, Cause} ->
                    ?error("notify_fun/3", [{cause, Cause}]);
                timeout = Cause->
                    ?error("notify_fun/3", [{cause, Cause}])
            end
    end;
notify_fun(?NODE_TYPE_GATEWAY,_Node,_BucketName) ->
    ok;
notify_fun(_,_,_) ->
    ok.


%% @private
after_completion([]) ->
    ok;
after_completion([{BucketName, DelBucketStateList}|Acc]) ->
    Ret = lists:all(fun(#del_bucket_state{state = State}) ->
                            State == ?STATE_FINISHED
                    end, DelBucketStateList),
    case Ret of
        true ->
            case leo_manager_mnesia:bulk_delete_del_bucket_info(DelBucketStateList) of
                ok ->
                    ?info("after_completion/1", [{"msg: dequeued and removed", BucketName}]);
                {error, Cause} ->
                    SummaryCause = ?ERROR_FAILED_REMOVING_DEL_BUCKET_MSG,
                    ?error("after_completion/1", [{summary_cause, SummaryCause},
                                                  {cause, Cause}])
            end;
        false ->
            void
    end,
    after_completion(Acc).
