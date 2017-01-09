%%======================================================================
%%
%% Network Lock Manager written in Erlang
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
-module(leo_nlm_lock_behaviour).

-include("leo_gateway.hrl").

-callback(start_link(Args::any()) ->
                {ok, any()}).

-callback(test(FileHandler::binary(), Lock::#lock_record{}) ->
                ok | {error, #lock_record{}} | {error, any()}).

-callback(lock(FileHandler::binary(), Lock::#lock_record{}) ->
                ok | {error, #lock_record{}} | {error, any()}).

-callback(unlock(FileHandler::binary(), Owner::binary(), Start::non_neg_integer(), End::integer()) ->
                ok).
