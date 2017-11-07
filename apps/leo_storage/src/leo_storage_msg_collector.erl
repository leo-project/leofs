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
%% ---------------------------------------------------------------------
%% Leo Storage - Message Collector (error notification)
%%
%% @doc Leo Storage's Message Receiver
%% @reference https://github.com/leo-project/leo_storage/blob/master/src/leo_storage_msg_collector.erl
%% @end
%%======================================================================
-module(leo_storage_msg_collector).

-behaviour(gen_server).

-include("leo_storage.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0, stop/0]).
-export([clear/0,
         get/0,
         set_enabled/0,
         notify/3, notify/4
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-undef(TIMEOUT).
-define(TIMEOUT, timer:seconds(5)).
-record(state, {
          messages = [] :: [term()],
          enabled = false :: boolean(),
          interval = ?TIMEOUT :: pos_integer()
         }).


%%====================================================================
%% API
%%====================================================================
%% @doc Starts the server
%%
-spec(start_link() ->
             {ok, pid()} | {error, any()}).
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% @doc Stop this server
-spec(stop() ->
             ok).
stop() ->
    gen_server:call(?MODULE, stop, ?TIMEOUT).


%% @doc Clear all messages
clear() ->
    gen_server:call(?MODULE, clear, ?TIMEOUT).


%% @doc Retrieve all messages
get() ->
    gen_server:call(?MODULE, get, ?TIMEOUT).

%% @doc Make it work to store messages
set_enabled() ->
    gen_server:call(?MODULE, set_enabled, ?TIMEOUT).

%% @doc Operate the data
-spec(notify(Msg, Method, Key) ->
             ok | {error, any()} when Msg::atom(),
                                      Method::put|get|delete,
                                      Key::binary()).
notify(Msg, Method, Key) ->
    gen_server:call(?MODULE, {notify, Msg, Method, Key}, ?TIMEOUT).

-spec(notify(Msg, Method, Key, ProcessingTime) ->
             ok | {error, any()} when Msg::atom(),
                                      Method::put|get|delete,
                                      Key::binary(),
                                      ProcessingTime::non_neg_integer()).
notify(Msg, Method, Key, ProcessingTime) ->
    gen_server:call(?MODULE, {notify, Msg, Method, Key, ProcessingTime}, ?TIMEOUT).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
%% @doc Initiates the server
init([]) ->
    {ok, #state{messages = dict:new()}}.


%% @doc gen_server callback - Module:handle_call(Request, From, State) -> Result
handle_call(stop, _From, State) ->
    {stop, shutdown, ok, State};

handle_call(clear, _From, State) ->
    {reply, ok, State#state{messages = dict:new()}};

handle_call(get, _From, #state{messages = Msg} = State) ->
    Ret = dict:to_list(Msg),
    {reply, {ok, Ret}, State};

handle_call(set_enabled, _From, State) ->
    {reply, ok, State#state{enabled = true}};

handle_call({notify, ?ERROR_MSG_TIMEOUT, _Method, _Key},
            _From, #state{enabled = false} = State) ->
    {reply, disabled, State};
handle_call({notify, ?ERROR_MSG_TIMEOUT, Method, Key},
            _From, #state{messages = Messages} = State) ->
    Msg_1 = dict:append(?MSG_ITEM_TIMEOUT, {Method, Key}, Messages),
    {reply, ok, State#state{messages = Msg_1}};
handle_call({notify,_Msg,_Method,_Key}, _From, State) ->
    {reply, ok, State};

handle_call({notify, ?ERROR_MSG_SLOW_OPERATION, _Method, _Key, _ProcessingTime},
            _From, #state{enabled = false} = State) ->
    {reply, disabled, State};
handle_call({notify, ?ERROR_MSG_SLOW_OPERATION, Method, Key, ProcessingTime},
            _From, #state{messages = Messages} = State) ->
    Msg_1 = dict:append(?MSG_ITEM_SLOW_OP, {Method, Key, ProcessingTime}, Messages),
    {reply, ok, State#state{messages = Msg_1}};
handle_call({notify,_Msg,_Method,_Key,_ProcessingTime},_From, State) ->
    {reply, ok, State};

handle_call(_Msg, _From, State) ->
    {reply, ok, State}.


%% @doc Handling cast message
%% <p>
%% gen_server callback - Module:handle_cast(Request, State) -> Result.
%% </p>
handle_cast(_Msg, State) ->
    {noreply, State}.


%% @doc Handling all non call/cast messages
%% <p>
%% gen_server callback - Module:handle_info(Info, State) -> Result.
%% </p>
handle_info(_Info, State) ->
    {noreply, State}.


%% @doc This function is called by a gen_server when it is about to
%%      terminate. It should be the opposite of Module:init/1 and do any necessary
%%      cleaning up. When it returns, the gen_server terminates with Reason.
terminate(_Reason,_State) ->
    ok.


%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
