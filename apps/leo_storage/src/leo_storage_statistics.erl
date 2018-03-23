%%======================================================================
%%
%% LeoStorage
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
%% ---------------------------------------------------------------------
%% Leo Storage  - Statistics
%% @doc
%% @end
%%======================================================================
-module(leo_storage_statistics).

-behaviour(leo_statistics_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-undef(MAX_RETRY_TIMES).
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1]).

%% callback
-export([handle_notify/0]).

-define(SNMP_MQ_NUM_OF_REPLICATION, 'mq-num-of-msg-replicate').
-define(SNMP_MQ_NUM_OF_SYNC_VNODE, 'mq-num-of-msg-sync-vnode').
-define(SNMP_MQ_NUM_OF_REBALANCE, 'mq-num-of-msg-rebalance').
-define(SNMP_MQ_NUM_OF_RECOVERY_NODE, 'mq-num-of-msg-recovery-node').
-define(SNMP_MQ_NUM_OF_DEL_DIR, 'mq-num-of-msg-deletion-dir').
-define(SNMP_MQ_NUM_OF_ASYNC_DEL_DIR, 'mq-num-of-msg-async-deletion-dir').
-define(SNMP_MQ_NUM_OF_REQ_DEL_DIR, 'mq-num-of-msg-req-deletion-dir').
-define(SNMP_MQ_MDCR_NUM_OF_COMP_METADATA, 'mq-mdcr-num-of-msg-req-comp-metadata').
-define(SNMP_MQ_MDCR_NUM_OF_REQ_SYNC_OBJ, 'mq-mdcr-num-of-msg-req-sync-obj').

-define(SNMP_COMP_STATE, 'comp-state').
-define(SNMP_COMP_LAST_START_DATETIME, 'comp-last-start-datetime').
-define(SNMP_COMP_LAST_END_DATETIME, 'comp-last-end-datetime').
-define(SNMP_COMP_NUM_OF_PENDING_TARGETS, 'comp-num-of-pending-targets').
-define(SNMP_COMP_NUM_OF_ONGOING_TARGETS, 'comp-num-of-ongoing-targets').
-define(SNMP_COMP_NUM_OF_OUT_OF_TARGETS, 'comp-num-of-out-of-targets').

-define(SNMP_STORAGE_ACTIVE_SIZE, 'storage-active-objects-sizes').
-define(SNMP_STORAGE_ACTIVE_OBJS, 'storage-active-objects').
-define(SNMP_STORAGE_TOTAL_SIZE, 'storage-total-objects-sizes').
-define(SNMP_STORAGE_TOTAL_OBJS, 'storage-total-objects').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
-spec(start_link(Window) ->
             ok when Window::non_neg_integer()).
start_link(Window) ->
    ok = leo_statistics_sup:start_child(?MODULE, Window),
    ok.


%%--------------------------------------------------------------------
%% Callbacks
%%--------------------------------------------------------------------
handle_notify() ->
    %% Set the total number of each mq item
    ok = get_and_set_mq_value(
           [{?SNMP_MQ_NUM_OF_REPLICATION, ?QUEUE_ID_PER_OBJECT},
            {?SNMP_MQ_NUM_OF_SYNC_VNODE, ?QUEUE_ID_SYNC_BY_VNODE_ID},
            {?SNMP_MQ_NUM_OF_REBALANCE, ?QUEUE_ID_REBALANCE},
            {?SNMP_MQ_NUM_OF_RECOVERY_NODE, ?QUEUE_ID_RECOVERY_NODE},
            {?SNMP_MQ_NUM_OF_DEL_DIR, ?QUEUE_ID_DEL_DIR},
            {?SNMP_MQ_NUM_OF_ASYNC_DEL_DIR, ?QUEUE_ID_ASYNC_DELETION},
            {?SNMP_MQ_NUM_OF_REQ_DEL_DIR, ?QUEUE_ID_REQ_DEL_DIR},
            {?SNMP_MQ_MDCR_NUM_OF_COMP_METADATA, ?QUEUE_ID_COMP_META_WITH_DC},
            {?SNMP_MQ_MDCR_NUM_OF_REQ_SYNC_OBJ, ?QUEUE_ID_SYNC_OBJ_WITH_DC}
           ]),

    %% Set the value of each data-compaction item
    ok = get_and_set_compaction_value(),

    %% Set the value of each storage item - Data compaction releated items
    ok = get_and_set_storage_value(),
    ok.


%% @private
get_and_set_mq_value([]) ->
    ok;
get_and_set_mq_value([{?SNMP_MQ_NUM_OF_DEL_DIR = Id, ?QUEUE_ID_DEL_DIR}|Rest]) ->
    V = lists:foldl(
          fun(QId, SoFar) ->
                  N = case catch leo_mq_api:status(QId) of
                          {ok, Ret} ->
                              leo_misc:get_value(
                                ?MQ_CNS_PROP_NUM_OF_MSGS, Ret, 0);
                          _ ->
                              0
                      end,
                  SoFar + N
          end, 0, ?del_dir_queue_list()),
    catch snmp_generic:variable_set(Id, V),
    get_and_set_mq_value(Rest);
get_and_set_mq_value([{Id, QId}|Rest]) ->
    V = case catch leo_mq_api:status(QId) of
            {ok, Ret} ->
                leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS, Ret, 0);
            _ ->
                0
        end,
    catch snmp_generic:variable_set(Id, V),
    get_and_set_mq_value(Rest).


%% @private
get_and_set_compaction_value() ->
    case leo_compact_fsm_controller:state() of
        {ok, #compaction_stats{
                status = Status,
                total_num_of_targets = NumOfTargets,
                num_of_pending_targets = NumOfPendingTargets,
                num_of_ongoing_targets = NumOfOnGoinfTargets}} ->
            {LastStartDT_1, LastEndDT_1} =
                case leo_object_storage_api:stats() of
                    {ok, []} ->
                        {0,0};
                    {ok, RetL} ->
                        lists:foldl(
                          fun(#storage_stats{compaction_hist = Histories},
                              {LastStartDT, LastEndDT}) ->
                                  case (length(Histories) == 0) of
                                      true ->
                                          {LastStartDT, LastEndDT};
                                      false ->
                                          #compaction_hist{start_datetime = S,
                                                           end_datetime = E} = hd(Histories),
                                          {max(LastStartDT, S), max(LastEndDT, E)}
                                  end;
                             (_, SoFar) ->
                                  SoFar
                          end, {0,0}, RetL);
                    _ ->
                        {0,0}
                end,

            [catch snmp_generic:variable_set(Id, V) ||
                {Id, V} <- [{?SNMP_COMP_STATE, to_compaction_state_int(Status)},
                            {?SNMP_COMP_LAST_START_DATETIME, to_unixtime(LastStartDT_1)},
                            {?SNMP_COMP_LAST_END_DATETIME, to_unixtime(LastEndDT_1)},
                            {?SNMP_COMP_NUM_OF_PENDING_TARGETS, NumOfPendingTargets},
                            {?SNMP_COMP_NUM_OF_ONGOING_TARGETS, NumOfOnGoinfTargets},
                            {?SNMP_COMP_NUM_OF_OUT_OF_TARGETS,
                             (NumOfTargets - (NumOfPendingTargets + NumOfOnGoinfTargets))}]];
        _ ->
            void
    end,
    ok.


%% @private
get_and_set_storage_value() ->
    {ok, Stats} = leo_object_storage_api:stats(),
    {TSizeRet, ASizeRet, TObjsRet, AObjsRet} =
        lists:foldl(
          fun(#storage_stats{
                 total_sizes = TSize_1,
                 active_sizes = ASize_1,
                 total_num = TObjs_1,
                 active_num = AObjs_1}, {TSize, ASize, TObjs, AObjs}) ->
                  {TSize_1 + TSize,
                   ASize_1 + ASize,
                   TObjs_1 + TObjs,
                   AObjs_1 + AObjs};
             (_, Acc) ->
                  Acc
          end, {0,0,0,0}, Stats),

    [catch snmp_generic:variable_set(Id, V) ||
        {Id, V} <- [{?SNMP_STORAGE_TOTAL_SIZE,  erlang:round(TSizeRet / 1024 / 1024)},
                    {?SNMP_STORAGE_ACTIVE_SIZE, erlang:round(ASizeRet / 1024 / 1024)},
                    {?SNMP_STORAGE_TOTAL_OBJS,  check_number(TObjsRet)},
                    {?SNMP_STORAGE_ACTIVE_OBJS, check_number(AObjsRet)}]],
    ok.


%% @private
to_compaction_state_int(?ST_IDLING) -> 0;
to_compaction_state_int(?ST_RUNNING) -> 1;
to_compaction_state_int(?ST_SUSPENDING) -> 2.


%% @private
to_unixtime(DateTime) ->
    case leo_date:greg_seconds_to_unixtime(DateTime) of
        UnixTime when UnixTime < 0 ->
            0;
        UnixTime ->
            UnixTime
    end.


%% @private
check_number(V) ->
    case (leo_math:power(2,32) =< V) of
        true ->
            4294967296;
        false when V < 0 ->
            0;
        false ->
            V
    end.
