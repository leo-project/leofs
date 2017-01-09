%%======================================================================
%%
%% Leo Manager
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
%%
%%======================================================================
-module(leo_manager_mq_client).

-behaviour(leo_mq_behaviour).

-include("leo_manager.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/2, start/3, publish/3]).
-export([init/0, handle_call/1, handle_call/3]).


-define(SLASH, "/").
-define(MQ_MSG_PATH_REBALANCE, "1").
-define(DEF_MQ_PROP_MAX_INTERVAL, 15000).

-define(env_num_of_mq_procs(),
        case application:get_env(leo_manager, num_of_mq_procs) of
            {ok, NumOfMQProcs} -> NumOfMQProcs;
            _ -> 3
        end).
-define(env_mq_backend_db(),
        case application:get_env(leo_storage, mq_backend_db) of
            {ok, EnvMQBackendDB} ->
                EnvMQBackendDB;
                        _ ->
                ?DEF_BACKEND_DB
        end).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc create queues and launch mq-servers.
%%
-spec(start(string(), list(tuple())) ->
             ok | {error, any()}).
start(RootPath, Intervals) ->
    start(leo_storage_sup, Intervals, RootPath).

-spec(start(pid(), list(tuple()), string()) ->
             ok | {error, any()}).
start(RefSup, Intervals, RootPath) ->
    %% launch mq-sup under storage-sup
    RefMqSup = case whereis(leo_mq_sup) of
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
    MaxInterval  = leo_misc:get_value(cns_interval_fail_rebalance_max,
                                      Intervals, ?DEF_CONSUME_MAX_INTERVAL),
    RegInterval  = leo_misc:get_value(cns_interval_fail_rebalance_regular,
                                      Intervals, ?DEF_CONSUME_REG_INTERVAL),
    RootPath_1 = case (string:len(RootPath) == string:rstr(RootPath, ?SLASH)) of
                     true  -> RootPath;
                     false -> RootPath ++ ?SLASH
                 end,
    leo_mq_api:new(RefMqSup, ?QUEUE_ID_FAIL_REBALANCE,
                   [{?MQ_PROP_MOD, ?MODULE},
                    {?MQ_PROP_FUN, ?MQ_SUBSCRIBE_FUN},
                    {?MQ_PROP_ROOT_PATH, RootPath_1 ++ ?MQ_MSG_PATH_REBALANCE},
                    {?MQ_PROP_DB_NAME,   ?env_mq_backend_db()},
                    {?MQ_PROP_DB_PROCS,  ?env_num_of_mq_procs()},
                    {?MQ_PROP_INTERVAL_MAX,    MaxInterval},
                    {?MQ_PROP_INTERVAL_REG,    RegInterval},
                    {?MQ_PROP_BATCH_MSGS_MAX,  ?DEF_CONSUME_MAX_BATCH_MSGS},
                    {?MQ_PROP_BATCH_MSGS_REG,  ?DEF_CONSUME_REG_BATCH_MSGS}
                   ]),
    ok.


%% @doc Input a message into the queue.
%%
-spec(publish(atom(), atom(), list()) ->
             ok | {error, any()}).
publish(?QUEUE_ID_FAIL_REBALANCE = Id, Node, RebalanceInfo) ->
    KeyBin     = term_to_binary(Node),
    MessageBin = term_to_binary(
                   #recovery_rebalance_info{id   = leo_date:clock(),
                                            node = Node,
                                            rebalance_info = RebalanceInfo,
                                            timestamp = leo_date:now()}),
    leo_mq_api:publish(Id, KeyBin, MessageBin);
publish(_,_,_) ->
    {error, badarg}.


%% -------------------------------------------------------------------
%% Callbacks
%% -------------------------------------------------------------------
%% @doc Initializer
%%
-spec(init() ->
             ok | {error, any()}).
init() ->
    ok.


%% @doc Subscribe a message from the queue.
%%
-spec(handle_call({publish | consume, atom(), binary()}) ->
             ok | {error, any()}).
handle_call({publish, _Id, _Reply}) ->
    ok;

handle_call({consume, ?QUEUE_ID_FAIL_REBALANCE, MessageBin}) ->
    case catch binary_to_term(MessageBin) of
        {'EXIT', Cause} ->
            ?error("handle_call/1 - QUEUE_ID_FAIL_REBALANCE",
                   [{cause, Cause}]),
            {error, Cause};
        #recovery_rebalance_info{node = Node,
                                 rebalance_info = RebalanceInfo} ->
            recover_rebalance(Node, RebalanceInfo)
    end.

-spec(handle_call(atom(), atom(), any()) ->
                 ok | {error, any()}).
handle_call(_,_,_) ->
    ok.


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @private
recover_rebalance(Node, RebalanceInfo) ->
    Ret = case leo_manager_api:get_members_of_all_versions() of
              {ok, {MembersCur, MembersPrev}} ->
                  case rpc:call(Node, leo_storage_api, rebalance,
                                [RebalanceInfo, MembersCur, MembersPrev], ?DEF_TIMEOUT) of
                      {ok, Hashes} ->
                          {RingHashCur, RingHashPrev} = leo_misc:get_value(ring, Hashes),
                          _ = leo_manager_mnesia:update_storage_node_status(
                                update_chksum,
                                #node_state{node = Node,
                                            ring_hash_new = leo_hex:integer_to_hex(RingHashCur,  8),
                                            ring_hash_old = leo_hex:integer_to_hex(RingHashPrev, 8)}),
                          ok;
                      {_, Cause} ->
                          {error, Cause};
                      timeout = Cause ->
                          {error, Cause}
                  end;
              {error, Reason} ->
                  {error, Reason}
          end,
    case Ret of
        ok ->
            ok;
        {error, _} ->
            ?MODULE:publish(?QUEUE_ID_FAIL_REBALANCE, Node, RebalanceInfo)
    end.
