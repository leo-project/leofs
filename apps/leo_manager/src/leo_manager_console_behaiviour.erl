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
-module(leo_manager_console_behaiviour).

-include("leo_manager.hrl").

%% @doc Request from 'tcp_server_acceptor'
%%
-callback(handle_call(Socket::port(), Command::binary(), #state{}) ->
                 {reply, any(), #state{}} | {noreply, #state{}}).

%% @doc Check to have a command
%%
-callback(has_command(Command::binary()) ->
                 boolean()).
