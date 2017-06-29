%%======================================================================
%%
%% LeoManager
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
-module(tcp_server_acceptor).

%% External API
-export([start_link/5]).

%% Callbacks
-export([init/5, accept/4]).

-include("leo_manager.hrl").
-include("tcp_server.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").


%%-----------------------------------------------------------------------
%% External API
%%-----------------------------------------------------------------------
start_link({Locale, Name}, Socket, State, Module, Option) ->
    {ok, Pid} = proc_lib:start_link(
                  ?MODULE, init,
                  [self(), Socket, State, Module, Option]),
    case Locale of
        local ->
            register(Name, Pid);
        _ ->
            global:register_name(Name, Pid)
    end,
    {ok, Pid}.

%% ---------------------------------------------------------------------
%% Callbacks
%% ---------------------------------------------------------------------
init(Parent, Socket, State, Module, Option) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    accept(Socket, State, Module, Option).


%% @private
accept(ListenSocket, State, Module, Option) ->
    case gen_tcp:accept(ListenSocket, Option#tcp_server_params.accept_timeout) of
        {ok, Socket} ->
            try
                recv(leo_misc:get_value(active, Option#tcp_server_params.listen),
                     Socket, State, Module, Option)
            catch
                Type:Reason ->
                    io:format("[error] ~p:~p - ~p,~p,~p~n",
                              [?MODULE, "accept/4", Module, Type, Reason])
            after
                gen_tcp:close(Socket)
            end;
        {error, Reason} ->
            io:format("[error] ~p:~p - ~p,~p~n",
                      [?MODULE, "accept/4", Module, Reason]),
            timer:sleep(Option#tcp_server_params.accept_error_sleep_time)
    end,
    accept(ListenSocket, State, Module, Option).


%% @private
recv(false, Socket, #state{auth = ?AUTH_NOT_YET,
                           formatter =?MOD_TEXT_FORMATTER} = State, Module, Option) ->
    call(false, Socket, ?USER_ID, State#state{auth = ?AUTH_USERID_1}, Module, Option);
recv(false, Socket, #state{auth = ?AUTH_USERID_2,
                           formatter =?MOD_TEXT_FORMATTER} = State, Module, Option) ->
    call(false, Socket, ?PASSWORD, State#state{auth = ?AUTH_PASSWORD}, Module, Option);

recv(false, Socket, State, Module, Option) ->
    AuthSt = State#state.auth,
    Formatter = State#state.formatter,

    case gen_tcp:recv(Socket,
                      Option#tcp_server_params.recv_length,
                      Option#tcp_server_params.recv_timeout) of
        {ok, Data} when AuthSt == ?AUTH_USERID_1 andalso
                        Formatter == ?MOD_TEXT_FORMATTER ->
            UserId = hd(string:tokens(
                          binary_to_list(Data), ?COMMAND_DELIMITER)),
            recv(false, Socket, State#state{user_id = list_to_binary(UserId),
                                            auth    = 2}, Module, Option);
        {ok, Data} when AuthSt == ?AUTH_PASSWORD andalso
                        Formatter == ?MOD_TEXT_FORMATTER ->
            UserId   = State#state.user_id,
            Password = hd(string:tokens(
                            binary_to_list(Data), ?COMMAND_DELIMITER)),

            case leo_s3_user:auth(UserId, list_to_binary(Password)) of
                {ok, _} ->
                    call(false, Socket, ?AUTHORIZED,
                         State#state{auth = ?AUTH_DONE}, Module, Option);
                {error, _} ->
                    recv(false, Socket, State#state{user_id = UserId,
                                                    auth = ?AUTH_NOT_YET}, Module, Option)
            end;
        {ok, Data} ->
            call(false, Socket, Data, State, Module, Option);
        {error, closed} ->
            tcp_closed;
        {error, Reason} ->
            ?warn("recv/5", [{cause, Reason}]),
            {error, Reason}
    end;

recv(true, _DummySocket, State, Module, Option) ->
    receive
        {tcp, Socket, Data} ->
            call(true, Socket, Data, State, Module, Option);
        {tcp_closed, _Socket} ->
            tcp_closed;
        {error, Reason} ->
            ?warn("recv/5", [{cause, Reason}]),
            {error, Reason}
    after Option#tcp_server_params.recv_timeout ->
            tcp_timeout
    end.


%% @private
call(Active, Socket, Data, #state{plugin_mod = PluginMod} = State, Module, Option) ->
    Ret = case PluginMod of
              undefined ->
                  false;
              _ ->
                  Tokens_1 = case binary:split(
                                    Data, [<<"\r\n">>, <<" ">>], [global,trim]) of
                                 [<<>>|Rest] ->
                                     Rest;
                                 Tokens ->
                                     Tokens
                             end,
                  case Tokens_1 of
                      [Command|_] ->
                          case catch PluginMod:has_command(Command) of
                              {'EXIT',_} ->
                                  false;
                              HasCommand ->
                                  HasCommand
                          end;
                      _ ->
                          false
                  end
          end,
    call_1(Ret, Active, Socket, Data, State, Module, Option).

call_1(HasCommand, Active, Socket, Data, State, Module, Option) ->
    Module_1 = case HasCommand of
                   true ->
                       State#state.plugin_mod;
                   false ->
                       Module
               end,

    case Module_1:handle_call(Socket, Data, State) of
        {reply, DataToSend, NewState} ->
            ?put_cmd_history(Data),
            gen_tcp:send(Socket, DataToSend),
            recv(Active, Socket, NewState, Module, Option);
        {noreply, NewState} ->
            recv(Active, Socket, NewState, Module, Option);
        {close, State} ->
            tcp_closed;
        {close, DataToSend, State} ->
            gen_tcp:send(Socket, DataToSend);
        Other ->
            ?warn("call_1/7", [{cause, Other}])
    end.
