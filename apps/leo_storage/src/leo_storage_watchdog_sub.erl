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
%% Leo Storage  - Watchdog Subscriber
%% @doc
%% @end
%%======================================================================
-module(leo_storage_watchdog_sub).

-behaviour(leo_notify_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_watchdog/include/leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start/0]).

%% callback
-export([handle_notify/3,
         handle_notify/4
        ]).

-define(WD_SUB_ID_1, 'leo_watchdog_sub_1').
-define(WD_SUB_ID_2, 'leo_watchdog_sub_2').
-define(DEF_WAIT_TIME, 100).


-record(compaction_info, {
          node = undefined  :: atom(),
          total_size = 0    :: non_neg_integer(),
          active_size = 0   :: non_neg_integer(),
          history = []      :: [non_neg_integer()],
          state = undefined :: atom()
         }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec(start() ->
             ok).
start() ->
    ok = leo_watchdog_sup:start_subscriber(?WD_SUB_ID_1, [leo_watchdog_cpu,
                                                          leo_watchdog_disk,
                                                          leo_watchdog_cluster,
                                                          leo_storage_watchdog_msgs,
                                                          leo_watchdog_error
                                                         ], ?MODULE),
    ok = leo_watchdog_sup:start_subscriber(?WD_SUB_ID_2, [leo_storage_watchdog_fragment
                                                         ], ?MODULE),
    ok.


%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------
-spec(handle_notify(Id, Alarm, Unixtime) ->
             ok | {error, any()} when Id::atom(),
                                      Alarm::term(),
                                      Unixtime::non_neg_integer()).
handle_notify(?WD_SUB_ID_1 = Id,_Alarm,_Unixtime) ->
    case is_active_watchdog() of
        true ->
            ?debug("handle_notify/3",
                   "received an alarm - id:~p, alarm:~p", [Id, _Alarm]),
            leo_compact_fsm_controller:decrease(),
            leo_mq_api:decrease(?QUEUE_ID_PER_OBJECT),
            leo_mq_api:decrease(?QUEUE_ID_SYNC_BY_VNODE_ID),
            leo_mq_api:decrease(?QUEUE_ID_REBALANCE),
            leo_mq_api:decrease(?QUEUE_ID_ASYNC_DELETION),
            leo_mq_api:decrease(?QUEUE_ID_RECOVERY_NODE),
            leo_mq_api:decrease(?QUEUE_ID_SYNC_OBJ_WITH_DC),
            leo_mq_api:decrease(?QUEUE_ID_COMP_META_WITH_DC),
            %% leo_mq_api:decrease(?QUEUE_ID_DEL_DIR),
            ok;
        false ->
            ok
    end;
handle_notify(?WD_SUB_ID_2, #watchdog_alarm{state = #watchdog_state{
                                                       level = Level,
                                                       props = Props}},_Unixtime) ->
    %% Clear the current status
    ok = leo_storage_watchdog_fragment:clear(),
    %% Execution data-compacion or not
    case (Level >= ?WD_LEVEL_ERROR) of
        true ->
            case can_start_compaction() of
                true ->
                    timer:sleep(?DEF_WAIT_TIME),
                    ThisTime = leo_date:now(),
                    AutoCompactionInterval = ?env_auto_compaction_interval(),

                    case leo_compact_fsm_controller:state() of
                        {ok, #compaction_stats{status = ?ST_IDLING,
                                               pending_targets = PendingTargets,
                                               latest_exec_datetime = LastExecDataTime
                                              }} when PendingTargets /= [] andalso
                                                      (ThisTime - LastExecDataTime) >= AutoCompactionInterval ->
                            ProcsOfParallelProcessing = ?env_auto_compaction_parallel_procs(),

                            case leo_object_storage_api:compact_data(
                                   PendingTargets, ProcsOfParallelProcessing,
                                   fun leo_storage_handler_object:can_compact_object/2) of
                                ok ->
                                    Ratio = leo_misc:get_value('ratio', Props),
                                    ?debug("handle_notify/3",
                                           "run-data-compaction - level:~w, ratio:~w%", [Level, Ratio]),
                                    ok;
                                _ ->
                                    ok
                            end;
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end;
        false ->
            ok
    end;
handle_notify(_,_,_) ->
    ok.


-spec(handle_notify(Id, State, SafeTimes, Unixtime) ->
             ok | {error, any()} when Id::atom(),
                                      State::[{atom(), any()}],
                                      SafeTimes::non_neg_integer(),
                                      Unixtime::non_neg_integer()).
handle_notify(?WD_SUB_ID_1 = Id,_State,_SafeTimes,_Unixtime) ->
    case is_active_watchdog() of
        true ->
            ?debug("handle_notify/3",
                   "loosen_control_at_safe_count - id:~p, state:~p, times:~p",
                   [Id,_State,_SafeTimes]),
            leo_compact_fsm_controller:increase(),
            leo_mq_api:increase(?QUEUE_ID_PER_OBJECT),
            leo_mq_api:increase(?QUEUE_ID_SYNC_BY_VNODE_ID),
            leo_mq_api:increase(?QUEUE_ID_REBALANCE),
            leo_mq_api:increase(?QUEUE_ID_ASYNC_DELETION),
            leo_mq_api:increase(?QUEUE_ID_RECOVERY_NODE),
            leo_mq_api:increase(?QUEUE_ID_SYNC_OBJ_WITH_DC),
            leo_mq_api:increase(?QUEUE_ID_COMP_META_WITH_DC),
            leo_mq_api:increase(?QUEUE_ID_DEL_DIR),
            ok;
        false ->
            ok
    end;
handle_notify(?WD_SUB_ID_2,_State,_SafeTimes,_Unixtime) ->
    ok.


%%--------------------------------------------------------------------
%% Internal Function
%%--------------------------------------------------------------------
%% @private
is_active_watchdog() ->
    false == (?env_wd_cpu_enabled()  == false andalso
              ?env_wd_disk_enabled() == false).

%% @private
can_start_compaction() ->
    case leo_redundant_manager_api:get_members_by_status(?STATE_RUNNING) of
        {ok, Members} ->
            MaxNumOfNodes =
                case (erlang:round(erlang:length(Members)
                                   * ?env_auto_compaction_coefficient())) of
                    N when N < 1 ->
                        1;
                    N ->
                        N
                end,
            is_candidates(Members, 0, MaxNumOfNodes, []);
        _ ->
            false
    end.


%% @doc Retrieve candidate nodes of data-compaction
%% @private
is_candidates([],_,_,[]) ->
    false;
is_candidates([],_,MaxNumOfNodes, Acc) ->
    is_candidates_1(MaxNumOfNodes, Acc);
is_candidates(_, MaxNumOfNodes, MaxNumOfNodes,_Acc) ->
    false;
is_candidates([#member{node = Node}|Rest], CntRunningNode, MaxNumOfNodes, Acc) ->
    case get_candidates(Node, Acc, 0) of
        {IsRunning, Acc_1} ->
            CntRunningNode_1 = case IsRunning of
                                   true ->
                                       CntRunningNode + 1;
                                   false ->
                                       CntRunningNode
                               end,
            is_candidates(Rest, CntRunningNode_1, MaxNumOfNodes, Acc_1);
        _ ->
            is_candidates(Rest, CntRunningNode, MaxNumOfNodes, Acc)
    end.

%% @private
is_candidates_1(MaxNumOfNodes, Candidates) ->
    %% Count running nodes
    RunningNodes = length([_N || #compaction_info{
                                    node = _N,
                                    state = #compaction_stats{status = ?ST_RUNNING}
                                   } <- Candidates]),
    case (RunningNodes >= MaxNumOfNodes) of
        true ->
            false;
        false ->
            %% Retrieve cadidates
            case (MaxNumOfNodes - RunningNodes) of
                0 ->
                    false;
                MaxNumOfNodes_1 when MaxNumOfNodes_1 < 0->
                    false;
                MaxNumOfNodes_1 ->
                    Candidates_1 = is_candidates_2(Candidates, []),
                    Candidates_2 = lists:sort([{History, Node} ||
                                                  #compaction_info{node = Node,
                                                                   history = History}
                                                      <- Candidates_1]),
                    Candidates_3 = [Node_1 || {_History, Node_1} <- Candidates_2],
                    case Candidates_3 of
                        [] ->
                            false;
                        _NotEmpty ->
                            {Candidates_4, Others} = lists:split(MaxNumOfNodes_1, Candidates_3),
                            case Others of
                                [] ->
                                    void;
                                _ ->
                                    _ = rpc:multicall(Others, leo_storage_watchdog_fragment,
                                                      clear, [], ?TIMEOUT)
                            end,
                            lists:member(erlang:node(), Candidates_4)
                    end
            end
    end.

%% @private
is_candidates_2([], Acc) ->
    Acc;
is_candidates_2([#compaction_info{state = #compaction_stats{status = ?ST_RUNNING}}|Rest], Acc) ->
    is_candidates_2(Rest, Acc);
is_candidates_2([#compaction_info{total_size = SumTotalSize,
                                  active_size = SumActiveSize,
                                  history = CompactionDate,
                                  state = #compaction_stats{status = ?ST_IDLING}
                                 } = CompactionInfo|Rest], Acc) ->
    %% Check a comapction-interval of the node
    MaxCompactionDate = lists:max(CompactionDate),
    DiffCompactionDate = leo_date:now() - MaxCompactionDate,
    CompactionInterval = ?env_auto_compaction_interval(),
    Acc_1 = case (DiffCompactionDate >= CompactionInterval) of
                true ->
                    %% Check a fragmentation ratio
                    ActiveSizeRatio =
                        case (SumTotalSize > 0) of
                            true when SumActiveSize > 0  ->
                                erlang:round(SumActiveSize / SumTotalSize * 100);
                            _ ->
                                0
                        end,
                    ThresholdActiveSizeRatio = ?env_threshold_active_size_ratio(),
                    case (ActiveSizeRatio =< ThresholdActiveSizeRatio) of
                        true ->
                            [CompactionInfo|Acc];
                        false ->
                            Acc
                    end;
                false ->
                    Acc
            end,
    is_candidates_2(Rest, Acc_1).


%% @private
get_candidates(_,Acc, ?RETRY_TIMES) ->
    Acc;
get_candidates(Node, Acc, RetryTimes) ->
    case rpc:call(Node, leo_object_storage_api,
                  du_and_compaction_stats, [], ?DEF_REQ_TIMEOUT) of
        {ok, []} ->
            Acc;
        {ok, RetL} ->
            DUState = leo_misc:get_value('du', RetL, []),
            CompactionState = leo_misc:get_value('compaction', RetL, []),

            %% Check du-state of the node
            {SumTotalSize, SumActiveSize, CompactionDate} =
                lists:foldl(
                  fun({T,A,H}, {SumT, SumA, EndDataL}) ->
                          EndDate_1 = case H of
                                          [] ->
                                              0;
                                          _ ->
                                              lists:max([EndDate ||
                                                            #compaction_hist{
                                                               end_datetime = EndDate}
                                                                <- H])
                                      end,
                          {SumT + T, SumA + A, [EndDate_1|EndDataL]}
                  end, {0,0,[]},
                  [{TotalSize, ActiveSize, History} ||
                      #storage_stats{total_sizes = TotalSize,
                                     active_sizes = ActiveSize,
                                     compaction_hist = History} <- DUState]),
            {CompactionState#compaction_stats.status == ?ST_RUNNING,
             [#compaction_info{node = Node,
                               total_size = SumTotalSize,
                               active_size = SumActiveSize,
                               history = CompactionDate,
                               state = CompactionState}|Acc]};
        _ ->
            timer:sleep(timer:seconds(1)),
            get_candidates(Node, Acc, RetryTimes + 1)
    end.
