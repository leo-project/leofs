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
-module(leo_manager_formatter_commons).

-include("leo_manager.hrl").

-export([exchange_value/2]).

%% @doc Exchange value
-spec(exchange_value(Values, Ret) ->
             Ret when Values::[{Ret, any()}],
                      Ret::any()).
exchange_value([], Ret) ->
    Ret;
exchange_value([{Ret, NewRet}|_], Ret) ->
    NewRet;
exchange_value([{_,_}|Rest], Ret) ->
    exchange_value(Rest, Ret).

