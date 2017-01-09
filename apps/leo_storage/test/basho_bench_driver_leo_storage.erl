%%======================================================================
%%
%% LeoFS Storage
%%
%% Copyright (c) 2012
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
%% LeoFS - Test Driver for basho_bench
%% @doc
%% @end
%%======================================================================
-module(basho_bench_driver_leo_storage).

-export([new/1,
         run/4]).


-define(STORAGE, 'storage_0@127.0.0.1').
-define(COOKIE,  'air_on_g_string').

%% @doc initialize
%%
-spec(new(any()) ->
             ok).
new(_Id) ->
    [] = os:cmd("epmd -daemon"),
    Test0Node = list_to_atom("test_0@127.0.0.1"),
    net_kernel:start([Test0Node, longnames]),

    erlang:set_cookie(node(), ?COOKIE),
    Res = net_adm:ping(?STORAGE),
    io:format("res:~p~n", [Res]),
    {ok, null}.


%% @doc run.
%%
-spec(run(put, any(), any(), any()) ->
             {ok, any()} | {error, any(), any()}).
run(put, KeyGen, ValueGen, State) ->
    Key = "_test/_storage/" ++ integer_to_list(KeyGen()),
    Bin = ValueGen(),
    RingId = leo_hex:hex_to_integer(leo_hex:binary_to_hex(erlang:md5(Key))),

    RPCKey = rpc:async_call(
               ?STORAGE, leo_storage_handler_object,
               put, [RingId, Key, Bin, byte_size(Bin), 0, 0]),
    case rpc:nb_yield(RPCKey, 3000) of
        {value, ok} ->
            {ok, State};
        Error ->
            {error, Error, State}
    end.


%% %% @doc run.
%% %%
%% -spec(run(get, any(), any(), any()) ->
%%              {ok, any()} | {error, any(), any()}).
%% run(get, KeyGen, _ValueGen, State) ->
%%     Key = KeyGen(),
%%     case leo_redundant_manager_api:get_redundancies_by_key(integer_to_list(Key)) of
%%         {ok, _} ->
%%             {ok, State};
%%         {error, Reason} ->
%%             {error, Reason, State}
%%     end.

