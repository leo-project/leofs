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
%% @doc Watchdog for a Storage-cluster
%% @reference
%% @end
%%======================================================================
-module(leo_storage_cluster_monitor).

-include("leo_storage.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([check_cluster_state/0,
         get_node_stats/0
        ]).

-record(state, {
          compaction :: atom(), %% [idling,running,suspending]
          mq   = []  :: [{atom(), atom()}], %% [{mq_id, [idling,running,suspending]}]
          cpu  = []  :: [{atom(), non_neg_integer()}], %% [{item_id, level:0..100}]
          disk = []  :: [{atom(), non_neg_integer()}]  %$ [{item_id, level:0..100}]
         }).

-define(LEVEL_INACTIVE,  0).
-define(LEVEL_LOW,      25).
-define(LEVEL_MID,      50).
-define(LEVEL_HIGH,     75).
-define(LEVEL_FULL ,   100).
-define(REQ_TIMEOUT, timer:seconds(10)).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Check the cluster status
%%      1. Retrieves the cluster members
%%      2. compaction and mq status from each member(storage-node)
%%      3. disk and cpu status from each member(storage-node)
%%      4. judges level of cluster
-spec(check_cluster_state() ->
             {ok, Level} |
             {error, Error} when Level::non_neg_integer(),
                                 Error::any()).
check_cluster_state() ->
    %% Retrieves the cluster members
    case leo_redundant_manager_api:get_members() of
        {ok, Members} ->
            Members_1 = lists:foldl(
                          fun(#member{node  = Node,
                                      state = ?STATE_RUNNING}, Acc) when Node /= erlang:node() ->
                                  [Node|Acc];
                             (_, Acc) ->
                                  Acc
                          end, [], Members),
            case Members_1 of
                [] ->
                    {ok, 0};
                _ ->
                    Ref  = make_ref(),
                    From = self(),
                    Pid  = spawn(fun() ->
                                         loop(Members_1, From, Ref, 0)
                                 end),

                    ok = check_cluster_state_1(Members_1, Pid, Ref),
                    receive
                        {Ref, {ok, SumLevel}} ->
                            Level = erlang:round(SumLevel / erlang:length(Members_1)),
                            {ok, Level};
                        {Ref, Error} ->
                            Error
                    after
                        (?DEF_REQ_TIMEOUT + timer:seconds(1)) ->
                            {ok, 0}
                    end
            end;
        _ ->
            {ok, 0}
    end.


%% @private
loop([], From, Ref, SoFar) ->
    erlang:send(From, {Ref, {ok, SoFar}});
loop(Members, From, Ref, SoFar) ->
    receive
        {Ref, Node, Level} ->
            loop(lists:delete(Node, Members), From, Ref, SoFar + Level)
    after
        ?DEF_REQ_TIMEOUT ->
            erlang:send(From, {Ref, {error, timeout}})
    end.


%% @doc Call for retrieving status of each node
%% @private
-spec(check_cluster_state_1(Members, Pid, Ref) ->
             {ok, Level} |
             {error, Error} when Members::[#member{}],
                                 Pid::pid(),
                                 Ref::reference(),
                                 Level::non_neg_integer(),
                                 Error::any()).
check_cluster_state_1([],_Pid,_Ref) ->
    ok;
check_cluster_state_1([Node|Rest], Pid, Ref) when erlang:node() == Node ->
    check_cluster_state_1(Rest, Pid, Ref);
check_cluster_state_1([Node|Rest], Pid, Ref) ->
    erlang:spawn(fun() ->
                         check_cluster_state_2(Node, Pid, Ref)
                 end),
    check_cluster_state_1(Rest, Pid, Ref).


%% @doc -Retrieves compaction and mq status from each member(storage-node)
%%      - Retrieves disk and cpu status from each member(storage-node)
%%      - Judge level of node-status
%% @private
check_cluster_state_2(Node, Pid, Ref) ->
    Level = case rpc:call(Node, ?MODULE, get_node_stats, [], ?REQ_TIMEOUT) of
                {ok, #state{compaction = CompactionState,
                            cpu  = CPUState,
                            disk = DiskState}} ->
                    Level_1 = level_compaction(CompactionState),
                    Level_2 = level_cpu(CPUState),
                    Level_3 = level_disk(DiskState),
                    erlang:round((Level_1 + Level_2 + Level_3) / 3);
                _ ->
                    0
            end,
    erlang:send(Pid, {Ref, Node, Level}).


%% @doc Retrieve level of compaction
%% @private
level_compaction(State) ->
    case leo_misc:get_value('status', State) of
        'running' ->
            ?LEVEL_LOW;
        _ ->
            ?LEVEL_INACTIVE
    end.

%% @doc Retrieve level of cpu
%% @private
level_cpu(State) ->
    case leo_misc:get_value('cur_error_times', State) of
        1 ->
            ?LEVEL_HIGH;
        Times when Times > 1 ->
            ?LEVEL_FULL;
        _ ->
            ?LEVEL_INACTIVE
    end.

%% @doc Retrieve level of disk
%% @private
level_disk(State) ->
    case leo_misc:get_value('cur_error_times', State) of
        1 ->
            ?LEVEL_HIGH;
        Times when Times > 1 ->
            ?LEVEL_FULL;
        _ ->
            ?LEVEL_INACTIVE
    end.


%% @doc Retrives Compaction, MQ, Disk and CPU status
-spec(get_node_stats() ->
             {ok, #state{}} | {error, Cause} when Cause::any()).
get_node_stats() ->
    CompactionState = get_node_stats_1(compaction),
    CPUState  = get_node_stats_1(cpu),
    DiskState = get_node_stats_1(disk),
    {ok, #state{compaction = CompactionState,
                cpu  = CPUState,
                disk = DiskState
               }}.

%% @private
get_node_stats_1(compaction) ->
    case leo_compact_fsm_controller:state() of
        {ok, #compaction_stats{} = State} ->
            lists:zip(record_info(fields, compaction_stats), tl(tuple_to_list(State)));
        _ ->
            []
    end;
get_node_stats_1(cpu) ->
    case leo_watchdog_cpu:state() of
        not_found ->
            [];
        {ok, Val} ->
            Val
    end;
get_node_stats_1(disk) ->
    case leo_watchdog_disk:state() of
        not_found ->
            [];
        {ok, Val} ->
            Val
    end.
