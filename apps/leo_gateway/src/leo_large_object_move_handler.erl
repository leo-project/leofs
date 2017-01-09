%%======================================================================
%%
%% Leo Gateway Large Object MOVE Handler
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
-module(leo_large_object_move_handler).

-behaviour(gen_server).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application callbacks
-export([start_link/3, stop/1]).
-export([get_chunk_obj/1]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-undef(DEF_SEPARATOR).
-define(DEF_SEPARATOR, <<"\n">>).

-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, 30000).

-record(state, {key = <<>>            :: binary(),
                max_obj_len = 0       :: non_neg_integer(),
                iterator              :: leo_large_object_commons:iterator()
               }).


%%====================================================================
%% API
%%====================================================================
-spec(start_link(Key, Length, TotalChunk) ->
             ok | {error, any()} when Key::binary(),
                                      Length::non_neg_integer(),
                                      TotalChunk::non_neg_integer()).
start_link(Key, Length, TotalChunk) ->
    gen_server:start_link(?MODULE, [Key, Length, TotalChunk], []).

%% @doc Stop this server
%%
-spec(stop(Pid) ->
             ok when Pid::pid()).
stop(Pid) ->
    gen_server:call(Pid, stop, ?DEF_TIMEOUT).


%% @doc Retrieve a part of chunked object from the storage cluster
%%
-spec(get_chunk_obj(Pid) ->
             {ok, binary()} | {error, any()} | done when Pid::pid()).
get_chunk_obj(Pid) ->
    gen_server:call(Pid, get_chunk_obj, ?DEF_TIMEOUT).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([Key, Length, TotalChunk]) ->
    State = #state{
               key = Key,
               max_obj_len = Length,
               iterator = leo_large_object_commons:iterator_init(Key, TotalChunk)},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(get_chunk_obj, _From, #state{iterator = Iter} = State) ->
    {Key, Iter_2} = leo_large_object_commons:iterator_next(Iter),
    case Key of
        <<"">> ->
            {reply, done, State#state{iterator = Iter_2}};
        _ ->
            Ret = leo_gateway_rpc_handler:get(Key),
            {Reply, NewIterator} = get_chunk_obj(Ret, Key, Iter_2),
            {reply, Reply, State#state{iterator = NewIterator}}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Private Functions
%%====================================================================
%% @doc Retrieve chunk object
%% @private
get_chunk_obj({ok, #?METADATA{cnumber = 0}, Bin}, _Key, Iter) ->
    {{ok, Bin}, Iter};
get_chunk_obj({ok, #?METADATA{cnumber = TotalChunkedObjs}, _Bin}, Key, Iter) ->
    Iter_1 = leo_large_object_commons:iterator_set_chunked(
               Iter, Key, TotalChunkedObjs),
    {Key_1, Iter_2} = leo_large_object_commons:iterator_next(Iter_1),

    case Key of
        <<"">> ->
            {done, Iter_2};
        _ ->
            case leo_gateway_rpc_handler:get(Key_1) of
                {ok, _, Bin2} ->
                    {{ok, Bin2}, Iter_2};
                {error, Cause} = Error ->
                    ?error("handle_call/3",
                           [{key, binary_to_list(Key_1)}, {cause, Cause}]),
                    {Error, Iter_2}
            end
    end;
get_chunk_obj({error, Cause} = Ret, Key, Iter) ->
    ?error("get_chunk_obj/3", [{key, Key}, {cause, Cause}]),
    {Ret, Iter}.
