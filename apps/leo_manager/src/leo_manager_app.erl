%%======================================================================
%%
%% Leo Manager
%%
%% Copyright (c) 2012-2015 Rakuten Inc.
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
-module(leo_manager_app).

-behaviour(application).

-include("leo_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application and Supervisor callbacks
-export([start/2, prep_stop/1, stop/1]).

%%----------------------------------------------------------------------
%% Application behaviour callbacks
%%----------------------------------------------------------------------
start(_Type, _Args) ->
    case leo_manager_sup:start_link() of
        {ok,_Pid} = Ret ->
            _ = timer:apply_after(?APPLY_AFTER_TIME, leo_manager_cluster_monitor,
                                  get_remote_node_proc, []),
            Ret;
        Other ->
            Other
    end.

prep_stop(_State) ->
    leo_redundant_manager_sup:stop(),
    leo_mq_sup:stop(),
    leo_logger_sup:stop(),
    ok.

stop(_State) ->
    ok.
