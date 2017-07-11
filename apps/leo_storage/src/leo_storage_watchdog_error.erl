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
%%======================================================================
-module(leo_storage_watchdog_error).

-include("leo_storage.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([push/1]).


%% @doc Push an error message to the watchdog-error
-spec(push(ErrorMsg) ->
             ok | {error, any()} when ErrorMsg::term()).
push(ErrorMsg) ->
    FormattedMsg = format(ErrorMsg),
    leo_watchdog_collector:push(FormattedMsg).


%% @doc Format a message
%% @private
format({error, Cause}) when is_tuple(Cause) ->
    format_tuple_value(Cause);
format({_, [Cause|_]}) when is_tuple(Cause) ->
    format_tuple_value(Cause);
format({_, Cause}) when is_tuple(Cause) ->
    format_tuple_value(Cause);
format(ErrorMsg) ->
    ErrorMsg.

%% @private
format_tuple_value(Item) ->
    case (erlang:size(Item) > 1) of
        true ->
            {erlang:element(1, Item),
             erlang:element(2, Item)};
        false ->
            Item
    end.
