%%======================================================================
%%
%% Leo Gateway
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
%% ---------------------------------------------------------------------
%% Leo Gateway - Cache Statistics
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_cache_statistics).

-behaviour(leo_statistics_behaviour).

-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("leo_cache/include/leo_cache.hrl").
-include_lib("eunit/include/eunit.hrl").

%% api
-export([start_link/1]).

%% callback
-export([handle_notify/0]).


-define(SNMP_MSG_REPLICATE,  'num-of-msg-replicate').
-define(SNMP_MSG_SYNC_VNODE, 'num-of-msg-sync-vnode').
-define(SNMP_MSG_REBALANCE,  'num-of-msg-rebalance').

-define(SNMP_CACHE_HIT_COUNT,  'cache-hit-count').
-define(SNMP_CACHE_MISS_COUNT, 'cache-miss-count').
-define(SNMP_CACHE_NUM_OF_OBJ, 'cache-object-count').
-define(SNMP_CACHE_TOTAL_SIZE, 'cache-object-size').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
start_link(Window) ->
    ok = leo_statistics_sup:start_child(?MODULE, Window),
    ok.

%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
handle_notify() ->
    Stats = case catch leo_cache_api:stats() of
                {ok, Value} ->
                    Value;
                {_,_Cause} ->
                    #stats{}
            end,
    #stats{get = NumOfRead,
           hits = HitCount,
           records = NumOfObjects,
           size = TotalOfSize} = Stats,
    TotalOfSize_1 = leo_math:ceiling(TotalOfSize / (1024*1024)),

    catch snmp_generic:variable_set(?SNMP_CACHE_HIT_COUNT, HitCount),
    catch snmp_generic:variable_set(?SNMP_CACHE_MISS_COUNT, NumOfRead - HitCount),
    catch snmp_generic:variable_set(?SNMP_CACHE_NUM_OF_OBJ, NumOfObjects),
    catch snmp_generic:variable_set(?SNMP_CACHE_TOTAL_SIZE, TotalOfSize_1),
    ok.
