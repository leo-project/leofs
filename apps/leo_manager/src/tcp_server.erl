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
-module(tcp_server).

-export([behaviour_info/1]).
-export([start_link/3, stop/0]).

-include("tcp_server.hrl").
-include_lib("eunit/include/eunit.hrl").


%% Behaviour Callbacks
behaviour_info(callbacks) ->
    [{init, 1}, {handle_call, 3}];
behaviour_info(_Other) ->
    undefined.


%% External APIs
start_link(Module, Args, Params) ->
    case Module:init(Args) of
        {ok, State}  ->
            case gen_tcp:listen(Params#tcp_server_params.port,
                                Params#tcp_server_params.listen) of
                {ok, Socket} ->
                    add_listener(Socket, State, Module, Params);
                {error, Reason} ->
                    {error, Reason}
            end;
        {stop, Reason} ->
            {error, Reason};
        _ ->
            {error, []}
    end.


stop() ->
    ok.


%% ---------------------------------------------------------------------
%% Internal Functions
%% ---------------------------------------------------------------------
add_listener(Socket, State, Module, Params) ->
    Index = Params#tcp_server_params.num_of_listeners,
    add_listener(Index, Socket, State, Module, Params).

add_listener(0,_,_,_,_) ->
    ok;
add_listener(Index, Socket, State, Module, Params) ->
    AcceptorName = list_to_atom(
                     lists:append([Params#tcp_server_params.prefix_of_name,
                                   integer_to_list(Index)])),
    ChildSpec = {AcceptorName,
                 {tcp_server_acceptor,
                  start_link, [{local, AcceptorName},
                               Socket,
                               State,
                               Module,
                               Params]},
                 permanent,
                 Params#tcp_server_params.shutdown,
                 worker,
                 [tcp_server_acceptor]},
    {ok, _Pid} = supervisor:start_child(tcp_server_sup, ChildSpec),
    add_listener(Index - 1, Socket, State, Module, Params).

