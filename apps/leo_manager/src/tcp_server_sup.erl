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
-module(tcp_server_sup).

-behaviour(supervisor).

-include("tcp_server.hrl").

%% External API
-export([start_link/0,
         stop/0]).

%% Callbacks
-export([init/1]).

%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
    case whereis(?MODULE) of
        Pid when is_pid(Pid) ->
            exit(Pid, shutdown),
            ok;
        _ ->
            not_started
    end.

%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 5, 60}, []}}.

