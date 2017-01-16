%%======================================================================
%%
%% Leo Gateway Large Object PUT Handler
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
-module(leo_large_object_put_handler).
-behaviour(gen_server).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").


%% Application callbacks
-export([start_link/2, start_link/3, stop/1]).
-export([put/2, rollback/1, result/1]).
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

-record(state, {bucket_info :: #?BUCKET{},
                key = <<>> :: binary(),
                max_obj_len = 0 :: non_neg_integer(),
                stacked_bin = <<>> :: binary(),
                num_of_chunks = 0 :: non_neg_integer(),
                total_len = 0 :: non_neg_integer(),
                md5_context = <<>> :: any(),
                errors = [] :: list(),
                monitor_set :: gw_set()
               }).


%%====================================================================
%% API
%%====================================================================
-spec(start_link(Key, Length) ->
             ok | {error, any()} when Key::binary(),
                                      Length::non_neg_integer()).
start_link(Key, Length) ->
    BucketName = erlang:hd(leo_misc:binary_tokens(Key, <<"/">>)),
    BucketInfo = #?BUCKET{name = BucketName},
    start_link(BucketInfo, Key, Length).

-spec(start_link(BucketInfo, Key, Length) ->
             ok | {error, any()} when BucketInfo::#?BUCKET{},
                                      Key::binary(),
                                      Length::non_neg_integer()).
start_link(BucketInfo, Key, Length) ->
    gen_server:start_link(?MODULE, [BucketInfo, Key, Length], []).


%% @doc Stop this server
%%
-spec(stop(Pid) ->
             ok when Pid::pid()).
stop(Pid) ->
    gen_server:call(Pid, stop, ?DEF_TIMEOUT).


%% @doc Insert a chunked object into the storage cluster
%%
-spec(put(Pid, Bin) ->
             ok | {error, any()} when Pid::pid(),
                                      Bin::binary()).
put(Pid, Bin) ->
    gen_server:call(Pid, {put, Bin}, ?DEF_TIMEOUT).


%% @doc Make a rollback before all operations
%%
-spec(rollback(Pid) ->
             ok | {error, any()} when Pid::pid()).
rollback(Pid) ->
    gen_server:call(Pid, rollback, ?DEF_TIMEOUT).


%% @doc Retrieve a result
%%
-spec(result(Pid) ->
             ok | {error, any()} when Pid::pid()).
result(Pid) ->
    gen_server:call(Pid, result, infinity).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
init([BucketInfo, Key, Length]) ->
    State = #state{bucket_info = BucketInfo,
                   key = Key,
                   max_obj_len = Length,
                   num_of_chunks = 1,
                   stacked_bin = <<>>,
                   md5_context = crypto:hash_init(md5),
                   errors = [],
                   monitor_set = sets:new()},
    {ok, State}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({put, Bin}, _From, #state{bucket_info = BucketInfo,
                                      key = Key,
                                      max_obj_len = MaxObjLen,
                                      stacked_bin = StackedBin,
                                      num_of_chunks = NumOfChunks,
                                      total_len = TotalLen,
                                      errors = [],
                                      monitor_set = MonitorSet,
                                      md5_context = Context} = State) ->
    Size = erlang:byte_size(Bin),
    TotalLen_1 = TotalLen + Size,
    Bin_1 = << StackedBin/binary, Bin/binary >>,

    case (erlang:byte_size(Bin_1) >= MaxObjLen) of
        true ->
            NumOfChunksBin = list_to_binary(integer_to_list(NumOfChunks)),
            << Bin_2:MaxObjLen/binary, StackedBin_1/binary >> = Bin_1,
            ChunkedKey = << Key/binary,
                            ?DEF_SEPARATOR/binary,
                            NumOfChunksBin/binary >>,
            {Ret, State_1} =
                case send_object(#put_req_params{path = ChunkedKey,
                                                 body = Bin_2,
                                                 dsize = MaxObjLen,
                                                 cindex = NumOfChunks,
                                                 bucket_info = BucketInfo}, leo_date:clock()) of
                    {ok, SpawnRet} ->
                        Context_1 = crypto:hash_update(Context, Bin_2),
                        << Head:8/binary, _Rest/binary>> = Bin_2,
                        Offset = MaxObjLen * NumOfChunks,
                        ?debug("handle_call",
                               "Save Chunk, Key: ~p, Chunk: ~p, Offset: ~p, Bin: ~p",
                               [Key, NumOfChunks, Offset, leo_hex:binary_to_hex(Head)]),
                        {ok, State#state{stacked_bin = StackedBin_1,
                                         num_of_chunks = NumOfChunks + 1,
                                         total_len = TotalLen_1,
                                         monitor_set = sets:add_element(SpawnRet, MonitorSet),
                                         md5_context = Context_1}};
                    {error, Cause} ->
                        {{error, Cause}, State#state{stacked_bin = StackedBin_1,
                                                     num_of_chunks = NumOfChunks + 1,
                                                     total_len = TotalLen_1,
                                                     monitor_set = MonitorSet}}
                end,
            {reply, Ret, State_1};
        false ->
            {reply, ok, State#state{stacked_bin = Bin_1,
                                    total_len = TotalLen_1}}
    end;

handle_call({put, _Bin}, _From, #state{key = Key,
                                       errors = Errors} = State) ->
    ?debug("handle_call", "Large Put Failed Key: ~p, Error: ~p", [Key, Errors]),
    {reply, {error, Errors}, State};

handle_call(rollback, _From, #state{key = Key} = State) ->
    catch leo_large_object_commons:delete_chunked_objects(Key),
    {reply, ok, State#state{errors = []}};


handle_call(result, _From, #state{bucket_info = BucketInfo,
                                  key = Key,
                                  md5_context = Context,
                                  stacked_bin = StackedBin,
                                  num_of_chunks = NumOfChunks,
                                  total_len = TotalLen,
                                  monitor_set = MonitorSet,
                                  errors = []} = State) ->
    Ret = case StackedBin of
              <<>> ->
                  {ok, {NumOfChunks - 1, Context}};
              _ ->
                  NumOfChunksBin = list_to_binary(integer_to_list(NumOfChunks)),
                  Size = erlang:byte_size(StackedBin),
                  Key_1 = << Key/binary,
                             ?DEF_SEPARATOR/binary,
                             NumOfChunksBin/binary >>,
                  case leo_gateway_rpc_handler:put(
                         #put_req_params{path = Key_1,
                                         body = StackedBin,
                                         dsize = Size,
                                         cindex = NumOfChunks,
                                         bucket_info = BucketInfo}) of
                      {ok,_ETag} ->
                          {ok, {NumOfChunks,
                                crypto:hash_update(Context, StackedBin)}};
                      {error, Cause} ->
                          {error, Cause}
                  end
          end,

    State_1 = State#state{stacked_bin = <<>>,
                          errors = []},
    case wait_sub_process(MonitorSet, []) of
        [] ->
            case Ret of
                {ok, {NumOfChunks_1, Context_1}} ->
                    Digest = crypto:hash_final(Context_1),
                    Reply = {ok, #large_obj_info{
                                    key = Key,
                                    num_of_chunks = NumOfChunks_1,
                                    length = TotalLen,
                                    md5_context = Digest}},
                    {reply, Reply, State_1#state{md5_context = Digest}};
                {error, Reason} ->
                    Reply = {error, {#large_obj_info{
                                        key = Key,
                                        num_of_chunks = NumOfChunks}, Reason}},
                    {reply, Reply, State_1}
            end;
        Errors ->
            {reply, {error, Errors}, State}
    end;

handle_call(result, _From, #state{key = Key,
                                  num_of_chunks = NumOfChunks,
                                  errors = Errors} = State) ->
    Cause = lists:reverse(Errors),
    Reply = {error, {#large_obj_info{key = Key,
                                     num_of_chunks = NumOfChunks}, Cause}},
    {reply, Reply, State#state{stacked_bin = <<>>,
                               errors = []}}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, #state{monitor_set = MonitorSet} = State) ->
    NewMonitorSet = sets:del_element({Pid, MonitorRef}, MonitorSet),
    {noreply, State#state{monitor_set = NewMonitorSet}};
handle_info({async_notify, Key, Ret} = _Info, #state{errors = Errors} = State) ->
    case Ret of
        {ok, _CheckSum} ->
            {noreply, State};
        {error, Cause} ->
            ?error("handle_info/2", [{key, binary_to_list(Key)}, {cause, Cause}]),
            {noreply, State#state{errors = [{error, Cause}|Errors]}}
    end.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% Private Functions
%%====================================================================
%% @doc Checkout a worker and execute the request
%% @private
-spec(send_object(PutReq, Times) ->
             {ok, SpawnRet} | {error, Cause} when PutReq::#put_req_params{},
                                                  Times::non_neg_integer(),
                                                  SpawnRet::{pid(), reference()},
                                                  Cause::any()).
send_object(PutReq, BeginTime) ->
    Key = PutReq#put_req_params.path,

    case leo_pod:checkout(?POD_LOH_WORKER) of
        {ok, Worker} ->
            Fun = fun() ->
                          Ret = case catch gen_server:call(Worker, {put, PutReq}, infinity) of
                                    {'EXIT', {Reason, _Stack}} ->
                                        {error, Reason};
                                    Result ->
                                        Result
                                end,
                          ok = leo_pod:checkin(?POD_LOH_WORKER, Worker),
                          erlang:send(self(), {async_notify, Key, Ret})
                  end,
            SpawnRet = erlang:spawn_monitor(Fun),
            {ok, SpawnRet};
        {error,_Cause} ->
            Latency = erlang:round((leo_date:clock() - BeginTime) / 1000),
            case (?DEF_REQ_TIMEOUT < Latency) of
                true ->
                    Cause = timeout,
                    erlang:send(self(), {async_notify, Key, {error, Cause}}),
                    ?error("send_object/2", [{key, Key}, {cause, Cause},
                                             {latency, Latency}]),
                    {error, Cause};
                false ->
                    timer:sleep(?DEF_WAIT_TIME_OF_CHECKOUT),
                    send_object(PutReq, BeginTime)
            end
    end.

%% @doc Waits sub processes
%% @private
wait_sub_process(MonitorSet, Errors) ->
    Size = sets:size(MonitorSet),
    case Size of
        0 ->
            Errors;
        _ ->
            receive
                {async_notify, _Key, {ok, _CheckSum}} ->
                    wait_sub_process(MonitorSet, Errors);
                {async_notify, Key, {error, Cause}} ->
                    ?error("wait_sub_process/2",
                           [{key, binary_to_list(Key)}, {cause, Cause}]),
                    wait_sub_process(MonitorSet, [{error, Cause}|Errors]);
                {'DOWN', MonitorRef, _Type, Pid, _Info} ->
                    NewMonitorSet = sets:del_element({Pid, MonitorRef}, MonitorSet),
                    wait_sub_process(NewMonitorSet, Errors);
                Other ->
                    ?error("wait_sub_process/2", [{unknown_message, Other}]),
                    wait_sub_process(MonitorSet, Errors)
            end
    end.
