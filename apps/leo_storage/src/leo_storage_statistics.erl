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

-define(SNMP_MSG_REPLICATE,   'num-of-msg-replicate').
-define(SNMP_MSG_SYNC_VNODE,  'num-of-msg-sync-vnode').
-define(SNMP_MSG_REBALANCE,   'num-of-msg-rebalance').
-define(SNMP_MSG_ACTIVE_SIZE, 'storage-active-objects-sizes').
-define(SNMP_MSG_ACTIVE_OBJS, 'storage-active-objects').
-define(SNMP_MSG_TOTAL_SIZE,  'storage-total-objects-sizes').
-define(SNMP_MSG_TOTAL_OBJS,  'storage-total-objects').


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
    %% set number of queues
    Num_1 = case catch leo_mq_api:status(?QUEUE_ID_PER_OBJECT) of
                {ok, Res_1} ->
                    leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS, Res_1, 0);
                _ -> 0
            end,
    Num_2 = case catch leo_mq_api:status(?QUEUE_ID_SYNC_BY_VNODE_ID) of
                {ok, Res_2} ->
                    leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS, Res_2, 0);
                _ -> 0
            end,
    Num_3 = case catch leo_mq_api:status(?QUEUE_ID_REBALANCE) of
                {ok, Res_3} ->
                    leo_misc:get_value(?MQ_CNS_PROP_NUM_OF_MSGS, Res_3, 0);
                _ -> 0
            end,

    catch snmp_generic:variable_set(?SNMP_MSG_REPLICATE,  Num_1),
    catch snmp_generic:variable_set(?SNMP_MSG_SYNC_VNODE, Num_2),
    catch snmp_generic:variable_set(?SNMP_MSG_REBALANCE,  Num_3),

    %% set size of stored data
    {ok, Ret4} = leo_object_storage_api:stats(),
    {TSize2, ASize2, TObjs2, AObjs2} =
        lists:foldl(fun(#storage_stats{total_sizes  = TSize0,
                                       active_sizes = ASize0,
                                       total_num    = TObjs0,
                                       active_num   = AObjs0},
                        {TSize1, ASize1, TObjs1, AObjs1}) ->
                            {TSize0 + TSize1,
                             ASize0 + ASize1,
                             TObjs0 + TObjs1,
                             AObjs0 + AObjs1};
                       (_, Acc) ->
                            Acc
                    end, {0,0,0,0}, Ret4),

    TObjs3 = check_number(TObjs2),
    AObjs3 = check_number(AObjs2),

    catch snmp_generic:variable_set(?SNMP_MSG_TOTAL_SIZE,  erlang:round(TSize2/1024/1024)),
    catch snmp_generic:variable_set(?SNMP_MSG_ACTIVE_SIZE, erlang:round(ASize2/1024/1024)),
    catch snmp_generic:variable_set(?SNMP_MSG_TOTAL_OBJS,  TObjs3),
    catch snmp_generic:variable_set(?SNMP_MSG_ACTIVE_OBJS, AObjs3),
    ok.

%% @private
check_number(Value) ->
    case (leo_math:power(2,32) =< Value) of
        true  -> 4294967296;
        false -> Value
    end.
