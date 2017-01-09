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
%%====================================================================
-module(leofs).

-include("leo_http.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start/0, stop/0,
         get/1, put/2, head/1, delete/1]).


start() ->
    application:start(crypto),
    application:start(ranch),
    application:start(asn1),
    application:start(leo_gateway).

stop() ->
    application:stop(leo_gateway).


%% @doc Retrieve an object
%%
-spec(get(binary()) ->
             {ok, {pos_integer(), binary()}} | not_found | {error, any()}).
get(Key) ->
    case catch leo_cache_api:get(Key) of
        not_found ->
            case leo_gateway_rpc_handler:get(Key) of
                {error,not_found} ->
                    not_found;
                {ok, #?METADATA{checksum = Checksum}, Body} ->
                    {ok, {Checksum, Body}};
                Error ->
                    Error
            end;
        {ok, CachedObj} ->
            #cache{etag = Checksum,
                   body = Body} = binary_to_term(CachedObj),
            {ok, {Checksum, Body}};
        {'EXIT', Cause} ->
            {error, Cause};
        Error ->
            Error
    end.


%% @doc Insert an object
%%
-spec(put(binary(), binary()) ->
             {ok, pos_integer()} | {error, any()}).
put(Key, Body) ->
    case leo_gateway_rpc_handler:put(#put_req_params{path = Key,
                                                     body = Body,
                                                     dsize = byte_size(Body)}) of
        {ok, ETag} = Ret ->
            Mime = leo_mime:guess_mime(Key),
            Val= term_to_binary(#cache{etag = ETag,
                                       mtime = leo_date:now(),
                                       content_type = Mime,
                                       body = Body,
                                       size = byte_size(Body)
                                      }),
            _ = (catch leo_cache_api:put(Key, Val)),
            Ret;
        Error ->
            Error
    end.


%% @doc Insert an object
%%
-spec(head(binary()) ->
             {ok, #?METADATA{}} | not_found | {error, any()}).
head(Key) ->
    case leo_gateway_rpc_handler:head(Key) of
        {ok, #?METADATA{del = 1}} ->
            not_found;
        Ret ->
            Ret
    end.


%% @doc Insert an object
%%
-spec(delete(binary()) ->
             ok | {error, any()}).
delete(Key) ->
    catch leo_cache_api:delete(Key),
    leo_gateway_rpc_handler:delete(Key).
