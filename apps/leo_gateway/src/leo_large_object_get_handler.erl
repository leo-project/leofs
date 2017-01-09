%%======================================================================
%%
%% Leo Gateway Large Object GET Handler
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
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
-module(leo_large_object_get_handler).

-behaviour(gen_server).
-behaviour(leo_tran_behaviour).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("leo_tran/include/leo_tran.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Application APIs
-export([start_link/1, stop/1, get/4]).

%% get_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Callbacks for leo_tran_behaviour
-export([run/5, resume/5, wait/5, commit/5, rollback/6]).

-undef(DEF_SEPARATOR).
-define(DEF_SEPARATOR, <<"\n">>).

-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, timer:seconds(30)).
-define(DEF_RETRY_TIMES, 5).

-record(state, {
          key = <<>> :: binary(),
          transport_rec :: #transport_record{},
          iterator :: leo_large_object_commons:iterator()
         }).

-record(req_info, {
          key = <<>> :: binary(),
          chunk_key = <<>>  :: binary(),
          request :: any(),
          metadata :: #?METADATA{},
          reference :: reference(),
          transport_rec :: #transport_record{}
         }).


%%====================================================================
%% API
%%====================================================================
-spec(start_link(Args) ->
             ok | {error, any()} when Args::tuple()).
start_link({Key, TransportRec}) ->
    gen_server:start_link(?MODULE, [Key, TransportRec], []).


%% @doc Stop this server
%%
-spec(stop(Pid) ->
             ok when Pid::pid()).
stop(Pid) ->
    gen_server:call(Pid, stop, ?DEF_TIMEOUT).


%% @doc Retrieve a chunked object from the storage cluster
%%
-spec(get(Pid, TotalOfChunkedObjs, Req, Meta) ->
             ok | {error, any()} when Pid::pid(),
                                      TotalOfChunkedObjs::non_neg_integer(),
                                      Req::any(),
                                      Meta::#?METADATA{}).
get(Pid, TotalOfChunkedObjs, Req, Meta) ->
    %% Since this call may take a long time in case of handling a very large file,
    %% Timeout sholud be infinity.
    gen_server:call(Pid, {get, TotalOfChunkedObjs, Req, Meta}, infinity).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([Key, TransportRec]) ->
    State = #state{key = Key,
                   transport_rec = TransportRec},
    {ok, State}.

handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({get, TotalOfChunkedObjs, Req, Meta}, _From,
            #state{key = Key, transport_rec = TransportRec} = State) ->
    Reply = case leo_tran_serializable_cntnr:run(
                   Key, null, null, ?MODULE,
                   {TotalOfChunkedObjs, Req, Meta, TransportRec},
                   [{?PROP_IS_WAIT_FOR_TRAN, false}], infinity) of
                {value, WriterRep} ->
                    WriterRep;
                {error, ?ERROR_ALREADY_HAS_TRAN} ->
                    case put_begin_tran_with_retry(Key) of
                        {ok, Ref} ->
                            TotalSize = Meta#?METADATA.dsize,
                            ReaderRep =
                                case catch handle_read_loop(
                                             TotalSize,
                                             #req_info{key = Key,
                                                       request = Req,
                                                       reference = Ref,
                                                       transport_rec = TransportRec}) of
                                    {'EXIT', Reason} ->
                                        {error, Reason};
                                    Ret ->
                                        Ret
                                end,
                            _ = leo_cache_api:put_end_tran(Ref, read, Key, undef, true),
                            ReaderRep;
                        {error, Cause} ->
                            {error, Cause}
                    end
            end,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Callbacks for leo_tran_behaviour
run(Key, _, _, {TotalOfChunkedObjs, Req, Meta, TransportRec}, _) ->
    Ref = case catch leo_cache_api:put_begin_tran(write, Key) of
              {ok, R} ->
                  R;
              _ ->
                  undefined
          end,
    Reply = handle_loop(TotalOfChunkedObjs,
                        #req_info{key = Key,
                                  chunk_key = Key,
                                  request = Req,
                                  metadata = Meta,
                                  reference = Ref,
                                  transport_rec = TransportRec}),
    case Reply of
        {ok, _Req} ->
            CacheMeta = #cache_meta{md5 = Meta#?METADATA.checksum,
                                    mtime = Meta#?METADATA.timestamp,
                                    content_type = leo_mime:guess_mime(Key)},
            catch leo_cache_api:put_end_tran(Ref, write, Key, CacheMeta, true);
        _ ->
            catch leo_cache_api:put_end_tran(Ref, write, Key, undefined, false)
    end,
    Reply.

wait(_,_,_,_,_) ->
    ok.
resume(_,_,_,_,_) ->
    ok.
commit(_,_,_,_,_) ->
    ok.
rollback(_,_,_,_,_,_) ->
    ok.


%%====================================================================
%% INNTERNAL FUNCTION
%%====================================================================
%% @doc Retrieve chunked objects
%% @private
-spec(handle_loop(TotalChunkObjs, ReqInfo) ->
             {ok, any()} | {error, any()} when TotalChunkObjs::non_neg_integer(),
                                               ReqInfo::#req_info{}).
handle_loop(TotalChunkObjs, ReqInfo) ->
    handle_loop(0, TotalChunkObjs, ReqInfo).


%% @private
handle_loop(TotalChunkObjs, TotalChunkObjs, #req_info{request = Req}) ->
    {ok, Req};
handle_loop(Index, TotalChunkObjs, #req_info{key = AcctualKey,
                                             chunk_key = ChunkObjKey,
                                             reference = Ref,
                                             transport_rec = TransportRec} = ReqInfo) ->
    IndexBin = list_to_binary(integer_to_list(Index + 1)),
    Key_1 = << ChunkObjKey/binary,
               ?DEF_SEPARATOR/binary,
               IndexBin/binary >>,

    ?debug("handle_loop/3", "Transport Record: ~p", [TransportRec]),
    case leo_gateway_rpc_handler:get(Key_1) of
        %%
        %% only children
        %%
        {ok, #?METADATA{cnumber = 0}, Bin} ->
            #transport_record{transport = Transport,
                              socket = Socket,
                              sending_chunked_obj_len = SendChunkLen} = TransportRec,
            case leo_net:chunked_send(Transport, Socket, Bin, SendChunkLen) of
                ok ->
                    catch leo_cache_api:put(Ref, AcctualKey, Bin),
                    leo_tran:notify_all(AcctualKey, null, null),
                    handle_loop(Index + 1, TotalChunkObjs, ReqInfo);
                {error, Cause} ->
                    ?error("handle_loop/3",
                           [{key, binary_to_list(Key_1)},
                            {index, Index}, {cause, Cause}]),
                    {error, Cause}
            end;

        %%
        %% both children and grand-children
        %%
        {ok, #?METADATA{cnumber = TotalChunkObjs_1}, _Bin} ->
            %% grand-children
            case handle_loop(0, TotalChunkObjs_1,
                             ReqInfo#req_info{chunk_key = Key_1}) of
                {ok, Req} ->
                    %% children
                    handle_loop(Index + 1, TotalChunkObjs,
                                ReqInfo#req_info{request = Req});
                {error, Cause} ->
                    ?error("handle_loop/3",
                           [{key, binary_to_list(Key_1)},
                            {index, Index}, {cause, Cause}]),
                    {error, Cause}
            end;
        {error, Cause} ->
            ?error("handle_loop/3",
                   [{key, binary_to_list(Key_1)},
                    {index, Index}, {cause, Cause}]),
            {error, Cause}
    end.


%% @doc Read Mode
%% @private
-spec(handle_read_loop(TotalSize, ReqInfo) ->
             {ok, any()} | {error, any()} when TotalSize::non_neg_integer(),
                                               ReqInfo::#req_info{}).
handle_read_loop(TotalSize, ReqInfo) ->
    handle_read_loop(0, TotalSize, ReqInfo).

%% @private
handle_read_loop(TotalSize, TotalSize, #req_info{request = Req}) ->
    {ok, Req};
handle_read_loop(Offset, TotalSize, #req_info{key = Key,
                                              reference = Ref,
                                              transport_rec = TransportRec} = ReqInfo) ->
    ReadSize = case file:read(Ref, 1024 * 1024) of
                   {ok, Bin} ->
                       #transport_record{transport = Transport,
                                         socket = Socket,
                                         sending_chunked_obj_len = SendChunkLen} = TransportRec,
                       leo_net:chunked_send(Transport, Socket, Bin, SendChunkLen),
                       byte_size(Bin);
                   eof ->
                       0;
                   {error, Reason} ->
                       ?error("handle_read_loop/3", [{ref, Ref}, {cause, Reason}]),
                       erlang:error(Reason)
               end,
    case ReadSize of
        0 ->
            leo_tran:wait(Key, null, null),
            handle_read_loop(Offset, TotalSize, ReqInfo);
        _ ->
            handle_read_loop(Offset + ReadSize, TotalSize, ReqInfo)
    end.


%% @doc Execute begin-tran
%% @private
put_begin_tran_with_retry(Key) ->
    put_begin_tran_with_retry(Key, ?DEF_RETRY_TIMES).

%% @private
put_begin_tran_with_retry(_,0) ->
    {error, ?ERROR_COULD_NOT_START_TRAN};
put_begin_tran_with_retry(Key, RetryTimes) ->
    case leo_cache_api:put_begin_tran(read, Key) of
        {ok, Ref} ->
            {ok, Ref};
        _ ->
            timer:sleep(timer:seconds(1)),
            put_begin_tran_with_retry(Key, RetryTimes - 1)
    end.
