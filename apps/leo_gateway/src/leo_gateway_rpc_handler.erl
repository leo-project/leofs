%%======================================================================
%%
%% Leo Gateway
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
%% ---------------------------------------------------------------------
%% Leo Gateway - RPC-Handler
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_rpc_handler).

-export([head/1,
         get/1,
         get/2,
         get/3,
         get_dir_meta/1,
         delete/1,
         put/1,
         invoke/5,
         get_request_parameters/2
        ]).

-include("leo_http.hrl").
-include("leo_gateway.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-undef(MAX_RETRY_TIMES).
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-record(rpc_params, {
          req_id = 0  :: non_neg_integer(),
          timestamp = 0  :: non_neg_integer(),
          addr_id = 0  :: non_neg_integer(),
          redundancies = [] :: [#redundant_node{}]
         }).


%% @doc Retrieve a metadata from the storage-cluster
%%
-spec(head(binary()) ->
             {ok, #?METADATA{}}|{error, any()}).
head(Key) ->
    ReqParams = get_request_parameters(head, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_object,
           head,
           [ReqParams#rpc_params.addr_id, Key],
           []).

%% @doc Retrieve an object from the storage-cluster
%%
-spec(get(binary()) ->
             {ok, #?METADATA{}, binary()}|{error, any()}).
get(Key) ->
    ok = leo_metrics_req:notify(?STAT_COUNT_GET),
    ReqParams = get_request_parameters(get, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_object,
           get,
           [ReqParams#rpc_params.addr_id, Key, ReqParams#rpc_params.req_id],
           []).
-spec(get(binary(), integer()) ->
             {ok, match}|{ok, #?METADATA{}, binary()}|{error, any()}).
get(Key, ETag) ->
    ok = leo_metrics_req:notify(?STAT_COUNT_GET),
    ReqParams = get_request_parameters(get, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_object,
           get,
           [ReqParams#rpc_params.addr_id, Key, ETag, ReqParams#rpc_params.req_id],
           []).

-spec(get(binary(), integer(), integer()) ->
             {ok, #?METADATA{}, binary()}|{error, any()}).
get(Key, StartPos, EndPos) ->
    ok = leo_metrics_req:notify(?STAT_COUNT_GET),
    ReqParams = get_request_parameters(get, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_object,
           get,
           [ReqParams#rpc_params.addr_id,
            Key, StartPos, EndPos,
            ReqParams#rpc_params.req_id],
           []).

%% @doc Retrieve a directory metadata encoded into binary from the storage-cluster
%%
-spec(get_dir_meta(binary()) ->
             {ok, binary()}|{error, any()}).
get_dir_meta(Key) ->
    ReqParams = get_request_parameters(get, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_directory,
           get,
           [Key],
           []).


%% @doc Remove an object from storage-cluster
%%
-spec(delete(binary()) ->
             ok|{error, any()}).
delete(Key) ->
    ok = leo_metrics_req:notify(?STAT_COUNT_DEL),
    ReqParams = get_request_parameters(delete, Key),
    invoke(ReqParams#rpc_params.redundancies,
           leo_storage_handler_object,
           delete,
           [#?OBJECT{addr_id = ReqParams#rpc_params.addr_id,
                     key = Key,
                     timestamp = ReqParams#rpc_params.timestamp},
            ReqParams#rpc_params.req_id],
           []).


%% @doc Insert an object into the storage-cluster (regular-case)
-spec(put(ReqParams) ->
             ok|{ok, pos_integer()}|{error, any()} when ReqParams::#put_req_params{}).
put(#put_req_params{path = Key,
                    body = Body,
                    dsize = Size,
                    meta = CMeta,
                    msize = MSize,
                    total_chunks = TotalChunks,
                    cindex = ChunkIndex,
                    csize = ChunkedSize,
                    digest = Digest
                    %% === NOTE: for 1.4.0 >>>
                    %% bucket_info = BucketInfo
                    %% <<<
                   }) ->
    ok = leo_metrics_req:notify(?STAT_COUNT_PUT),

    %% === NOTE: for 1.4.0 >>>
    %% #?BUCKET{redundancy_method = RedMethod,
    %%          cp_params = CPParams,
    %%          ec_lib = ECLib,
    %%          ec_params = ECParams} =
    %%     case BucketInfo of
    %%         undefined ->
    %%             BucketName =
    %%                 erlang:hd(
    %%                   leo_misc:binary_tokens(Key, <<"/">>)),
    %%             case catch leo_s3_bucket:get_latest_bucket(BucketName) of
    %%                 {ok, #?BUCKET{} = BucketInfo_1} ->
    %%                     BucketInfo_1;
    %%                 _ ->
    %%                     #?BUCKET{name = BucketName}
    %%             end;
    %%         _ ->
    %%             BucketInfo
    %%     end,
    %% <<<

    #rpc_params{addr_id = AddrId,
                redundancies = Redundancies,
                timestamp = Timestamp,
                req_id = ReqId} = get_request_parameters(put, Key),
    invoke(Redundancies, leo_storage_handler_object, put,
           [#?OBJECT{addr_id = AddrId,
                     key = Key,
                     data = Body,
                     meta = CMeta,
                     dsize = Size,
                     msize = MSize,
                     timestamp = Timestamp,
                     csize = ChunkedSize,
                     cnumber = TotalChunks,
                     cindex = ChunkIndex,
                     checksum = Digest
                     %% === NOTE: for 1.4.0 >>>
                     %% redundancy_method = RedMethod,
                     %% cp_params = CPParams,
                     %% ec_lib = ECLib,
                     %% ec_params = ECParams
                     %% <<<
                    }, ReqId], []).


%% @doc Do invoke rpc calls with handling retries
%%
-spec(invoke(list(), atom(), atom(), list(), list()) ->
             ok|{ok, any()}|{ok, #?METADATA{}, binary()}|{error, any()}).
invoke([], _Mod, _Method, _Args, Errors) ->
    {error, error_filter(Errors)};
invoke([#redundant_node{available = false}|T], Mod, Method, Args, Errors) ->
    invoke(T, Mod, Method, Args, [?ERR_TYPE_INTERNAL_ERROR|Errors]);
invoke([#redundant_node{node = Node,
                        available = true}|T], Mod, Method, Args, Errors) ->
    Timeout = timeout(Method, Args),
    case rpc:call(Node, Mod, Method, Args, Timeout) of
        %% is_dir
        Ret when is_boolean(Ret) ->
            Ret;
        %% delete
        ok = Ret ->
            Ret;
        %% put
        {ok, {etag, ETag}} ->
            {ok, ETag};
        %% get-1
        {ok, Meta, Bin} ->
            case leo_object_storage_transformer:transform_metadata(Meta) of
                {error,_} ->
                    {ok, Meta, Bin};
                Meta_1 ->
                    {ok, Meta_1, Bin}
            end;
        %% get-2
        {ok, match} = Ret ->
            Ret;
        %% find_by_parent_dir
        {ok, MetaL} when is_list(MetaL) ->
            TMetaL = lists:foldl(fun(Meta, Acc) ->
                                         Out = case leo_object_storage_transformer:transform_metadata(Meta) of
                                                   {error, _} ->
                                                       Meta;
                                                   Meta_1 ->
                                                       Meta_1
                                               end,
                                         [Out | Acc]
                                 end, [], MetaL),
            {ok, lists:reverse(TMetaL)};
        %% head/get_dir_meta
        {ok, Meta} ->
            case leo_object_storage_transformer:transform_metadata(Meta) of
                {error,_} ->
                    {ok, Meta};
                Meta_1 ->
                    {ok, Meta_1}
            end;
        {badrpc, _Cause} = Error ->
            E = handle_error(Node, Mod, Method, Args, Error),
            invoke(T, Mod, Method, Args, [E|Errors]);
        Error ->
            {error, handle_error(Node, Mod, Method, Args, Error)}
    end.

%% @doc Get request parameters
%%
-spec(get_request_parameters(atom(), binary()) ->
             #rpc_params{}).
get_request_parameters(Method, Key) ->
    {ok, #redundancies{id = Id,
                       nodes = Redundancies}} =
        leo_redundant_manager_api:get_redundancies_by_key(Method, Key),

    UnivDateTime = erlang:universaltime(),
    {_,_,NowPart} = os:timestamp(),
    {{Y,MO,D},{H,MI,S}} = UnivDateTime,

    ReqId = erlang:phash2([Y,MO,D,H,MI,S, erlang:node(), Key, NowPart]),
    Timestamp = calendar:datetime_to_gregorian_seconds(UnivDateTime),

    #rpc_params{addr_id = Id,
                redundancies = Redundancies,
                req_id = ReqId,
                timestamp = Timestamp}.


%% @doc Error messeage filtering
%%
error_filter([not_found|_])          -> not_found;
error_filter([{error, not_found}|_]) -> not_found;
error_filter([H|T])                  -> error_filter(T, H).
error_filter([],             Prev)   -> Prev;
error_filter([not_found|_T],_Prev)   -> not_found;
error_filter([{error,not_found}|_],_Prev) -> not_found;
error_filter([_H|T],                Prev) -> error_filter(T, Prev).


%% @doc Handle an error response
%%
handle_error(_Node,_Mod,_Method,_Args, {error, not_found = Error}) ->
    Error;
handle_error(_Node,_Mod,_Method,_Args, {error, unavailable = Error}) ->
    Error;
handle_error(Node, Mod, Method, _Args, {error, Cause}) ->
    ?warn("handle_error/5",
          [{node, Node}, {mod, Mod},
           {method, Method}, {cause, Cause}]),
    ?ERR_TYPE_INTERNAL_ERROR;
handle_error(Node, Mod, Method, _Args, {badrpc, Cause}) ->
    ?warn("handle_error/5",
          [{node, Node}, {mod, Mod},
           {method, Method}, {cause, Cause}]),
    ?ERR_TYPE_INTERNAL_ERROR;
handle_error(Node, Mod, Method, _Args, timeout = Cause) ->
    ?warn("handle_error/5",
          [{node, Node}, {mod, Mod},
           {method, Method}, {cause, Cause}]),
    Cause.


%% @doc Timeout depends on length of an object
%%
timeout(Len) when ?TIMEOUT_L1_LEN > Len -> ?env_timeout_level_1();
timeout(Len) when ?TIMEOUT_L2_LEN > Len -> ?env_timeout_level_2();
timeout(Len) when ?TIMEOUT_L3_LEN > Len -> ?env_timeout_level_3();
timeout(Len) when ?TIMEOUT_L4_LEN > Len -> ?env_timeout_level_4();
timeout(_)                              -> ?env_timeout_level_5().

timeout(put, [#?OBJECT{dsize = DSize}, _]) ->
    timeout(DSize);
timeout(get, _) -> ?env_timeout_for_get();
timeout(find_by_parent_dir, _) -> ?env_timeout_for_ls();
timeout(_, _) ->
    ?DEF_REQ_TIMEOUT.
