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
%% @doc Watchdog for Storage
%% @reference
%% @end
%%======================================================================
-module(leo_storage_watchdog_fragment).

-behaviour(leo_watchdog_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_watchdog/include/leo_watchdog.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3,
         stop/0
        ]).
-export([clear/0,
         state/0]).

%% Callback
-export([init/1,
         update_property/3,
         handle_call/2,
         handle_fail/2]).

-record(state, {
          warn_active_size_ratio      = ?DEF_WARN_ACTIVE_SIZE_RATIO      :: pos_integer(),
          threshold_active_size_ratio = ?DEF_THRESHOLD_ACTIVE_SIZE_RATIO :: pos_integer(),
          interval = timer:seconds(60) :: pos_integer()
         }).
-define(THRESHOLD_DIFFERENCE, 3).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start the server
-spec(start_link(WarnActiveSizeRatio, ThresholdActiveSizeRatio, Interval) ->
             {ok,Pid} |
             ignore |
             {error,Error} when WarnActiveSizeRatio::pos_integer(),
                                ThresholdActiveSizeRatio::pos_integer(),
                                Interval::pos_integer(),
                                Pid::pid(),
                                Error::{already_started,Pid} | term()).
start_link(WarnActiveSizeRatio, ThresholdActiveSizeRatio, Interval) ->
    State = #state{warn_active_size_ratio = WarnActiveSizeRatio,
                   threshold_active_size_ratio = ThresholdActiveSizeRatio,
                   interval = Interval},
    leo_watchdog:start_link(?MODULE, ?MODULE, State, Interval).


%% @doc Stop the server
-spec(stop() ->
             ok).
stop() ->
    leo_watchdog:stop(?MODULE).


%% @doc Clear an alert
-spec(clear() ->
             ok).
clear() ->
    elarm:clear(?MODULE, ?WD_ITEM_ACTIVE_SIZE_RATIO),
    ok.


%% @doc Retrieve state of the watchdog
-spec(state() ->
             not_found).
state() ->
    not_found.


%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Initialize this process
-spec(init(State) ->
             ok | {error, Cause} when State::any(),
                                      Cause::any()).
init(_State) ->
    ok.


%% @doc Update a property
-spec(update_property(Item, Value, State) ->
             ok | {error, any()} when Item::atom(),
                                      Value::any(),
                                      State::#state{}).
update_property(_,_,_) ->
    ok.


%% @dog Call execution of the watchdog
-spec(handle_call(Id, State) ->
             {ok, State} |
             {{error,Error}, State} when Id::atom(),
                                         State::#state{},
                                         Error::any()).
handle_call(Id, #state{warn_active_size_ratio = WarningThreshold,
                       threshold_active_size_ratio = AlartThreshold} = State) ->
    ok = handle_ratio_of_fragment(Id, WarningThreshold, AlartThreshold),
    {ok, State}.


%% @dog Call execution failed
-spec(handle_fail(Id, Cause) ->
             ok | {error,Error} when Id::atom(),
                                     Cause::any(),
                                     Error::any()).
handle_fail(_Id,_Cause) ->
    ok.


%%--------------------------------------------------------------------
%% Internal Function
%%--------------------------------------------------------------------
%% @doc Handle object-storage's fragment ratio for the data-compaction
%% @private
handle_ratio_of_fragment(Id, WarningThreshold, AlartThreshold) ->
    {ok, Stats} = leo_object_storage_api:stats(),
    {TotalSize, ActiveSize} =
        lists:foldl(fun(#storage_stats{total_sizes  = TSize,
                                       active_sizes = ASize},
                        {TSize_1, ASize_1}) ->
                            {TSize + TSize_1,
                             ASize + ASize_1};
                       (_, Acc) ->
                            Acc
                    end, {0,0}, Stats),

    Ratio = case (TotalSize > 0) of
                true ->
                    leo_math:ceiling(ActiveSize / TotalSize * 100);
                false ->
                    0
            end,

    case ((Ratio > 0 orelse (ActiveSize == 0 andalso TotalSize > 0))
          andalso Ratio =< WarningThreshold) of
        true when Ratio =< AlartThreshold ->
            %% raise error
            elarm:raise(Id, ?WD_ITEM_ACTIVE_SIZE_RATIO,
                        #watchdog_state{id = Id,
                                        level = ?WD_LEVEL_ERROR,
                                        src   = ?WD_ITEM_ACTIVE_SIZE_RATIO,
                                        props = [{ratio, Ratio}
                                                ]});
        true ->
            %% raise warning
            elarm:raise(Id, ?WD_ITEM_ACTIVE_SIZE_RATIO,
                        #watchdog_state{id = Id,
                                        level = ?WD_LEVEL_WARN,
                                        src   = ?WD_ITEM_ACTIVE_SIZE_RATIO,
                                        props = [{ratio, Ratio}
                                                ]});
        false ->
            elarm:clear(Id, ?WD_ITEM_ACTIVE_SIZE_RATIO)
    end,
    ok.
