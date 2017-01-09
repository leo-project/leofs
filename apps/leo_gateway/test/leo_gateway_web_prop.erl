%%====================================================================
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
%% -------------------------------------------------------------------
%% Leo Gateway - Property TEST
%% @doc
%% @end
%%====================================================================
-module(leo_gateway_web_prop).

-export([test/0, test/1]).
-export([prop_http_req/0]).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("proper/include/proper.hrl").


-define(TARGET_HOST, "localhost").
-define(TARGET_PORT, "8080").

%% @doc extend basic types
%%
method() -> union(['head', 'get', 'put', 'delete']).
result() -> union(['ok', 'not_found', 'timeout', 'error']).
digit_char() -> integer(16#30, 16#39).
alpha_char() -> integer(16#61, 16#7a).
uri_char() -> union([digit_char(), alpha_char()]).
bucket() -> list(uri_char()).
path()   -> list(uri_char()).

test() ->
    test(256).
test(N) ->
    proper:quickcheck(?MODULE:prop_http_req(), N).

prop_http_req() ->
    ?FORALL({Method, Bucket, Path, Body, Result},
            {method(), bucket(), path(), binary(), result()},
            begin
                Url = url_gen(Bucket, Path),
                Headers = headers_gen(),
                RawResp = raw_resp_gen(Method, Bucket, Path, Result, Body),
                meck_begin(Method, Bucket, Path, RawResp),
                io:format(user, "method:~p url:~s resp:~p~n", [Method, Url, RawResp]),
                try
                    Res = collect(Method, http_req(Method, Url, Headers, Body, RawResp)),
                    io:format("~p~n", [Res]),
                    Res
                catch
                    throw:Reason ->
                        io:format(user, "error:~p~n",[Reason]),
                        false
                after
                    meck_end(Bucket, Path)
                end
            end).

meck_begin(Method, Bucket, Path, RawResp) ->
    meck:new(leo_s3_auth, [non_strict]),
    meck:expect(leo_s3_auth, authenticate, 3, {ok, <<"AccessKey">>}),
    meck_begin_1(Method, Bucket, Path, RawResp).
meck_begin_1(Method, Bucket, Path, RawResp) when length(Bucket) > 0 andalso length(Path) > 0 ->
    meck:new(leo_gateway_rpc_handler, [non_strict]),
    meck:expect(leo_gateway_rpc_handler, Method, 1, RawResp),
    meck:expect(leo_gateway_rpc_handler, Method, 2, RawResp),
    meck:expect(leo_gateway_rpc_handler, Method, 3, RawResp),
    meck:expect(leo_gateway_rpc_handler, Method, 4, RawResp);
meck_begin_1('put', _Bucket, _Path, RawResp) ->
    meck:new(leo_s3_http_bucket, [non_strict]),
    meck:expect(leo_s3_http_bucket, put_bucket, 2, RawResp);
meck_begin_1('delete', _Bucket, _Path, RawResp) ->
    meck:new(leo_s3_http_bucket, [non_strict]),
    meck:expect(leo_s3_http_bucket, delete_bucket, 2, RawResp);
meck_begin_1('head', _Bucket, _Path, RawResp) ->
    meck:new(leo_s3_http_bucket, [non_strict]),
    meck:expect(leo_s3_http_bucket, head_bucket, 2, RawResp);
meck_begin_1('get', _Bucket, _Path, RawResp) ->
    meck:new(leo_s3_http_bucket, [non_strict]),
    meck:expect(leo_s3_http_bucket, get_bucket_list, 6, RawResp).

meck_end(Bucket, Path) ->
    meck:unload(leo_s3_auth),
    case length(Bucket) > 0 andalso length(Path) > 0 of
        true  -> meck:unload(leo_gateway_rpc_handler);
        false -> meck:unload(leo_s3_http_bucket)
    end.

http_req(Method, Url, Headers, _Body, RawResp) when Method =/= 'put' ->
    case httpc:request(Method, {Url, Headers}, [], [{body_format, binary}]) of
        {ok, {{_, SC, _}, RespHeaders, RespBody}} ->
            io:format(user, "sc:~p headers:~p body:~p~n",[SC, RespHeaders, RespBody]),
            http_check_resp(SC, RespHeaders, RespBody, Method, RawResp);
        {error, Reason} ->
            io:format(user, "error:~p~n",[Reason]),
            false
    end;
http_req(Method, Url, Headers, Body, RawResp) ->
    case httpc:request(Method, {Url, Headers, "text/plain", Body}, [], []) of
        {ok, {{_, SC, _}, RespHeaders, RespBody}} ->
            io:format(user, "sc:~p headers:~p body:~p~n",[SC, RespHeaders, RespBody]),
            http_check_resp(SC, RespHeaders, RespBody, Method, RawResp);
        {error, Reason} ->
            io:format(user, "error:~p~n",[Reason]),
            false
    end.

http_check_resp(SC, _RespHeaders, _RespBody, _Method, not_found) ->
    SC =:= 404;
http_check_resp(SC, _RespHeaders, _RespBody, _Method, {error, not_found}) ->
    SC =:= 404;
http_check_resp(SC, _RespHeaders, _RespBody, _Method, {error, ?ERR_TYPE_INTERNAL_ERROR}) ->
    SC =:= 500;
http_check_resp(SC, _RespHeaders, _RespBody, _Method, {error, timeout}) ->
    SC =:= 504;
http_check_resp(SC, _RespHeaders, _RespBody, 'delete', ok) ->
    SC =:= 204;
http_check_resp(SC, _RespHeaders, _RespBody, 'put', ok) ->
    SC =:= 200;
http_check_resp(SC, _RespHeaders, _RespBody, 'put', {ok, _ETag}) ->
    SC =:= 200;
http_check_resp(SC, _RespHeaders, _RespBody, 'head', ok) ->
    SC =:= 200;
http_check_resp(SC, RespHeaders, _RespBody, 'head', {ok, #?METADATA{dsize = DSize}}) ->
    ContentLength = list_to_integer(proplists:get_value("content-length", RespHeaders, "0")),
    SC =:= 200 andalso ContentLength =:= DSize;
http_check_resp(SC, RespHeaders, RespBody, 'get', {ok, #?METADATA{dsize = DSize}, Body}) ->
    ContentLength = list_to_integer(proplists:get_value("content-length", RespHeaders, "0")),
    SC =:= 200 andalso ContentLength =:= DSize andalso RespBody =:= Body;
http_check_resp(SC, RespHeaders, RespBody, 'get', {ok, _MetaList, Body}) ->
    ContentLength = list_to_integer(proplists:get_value("content-length", RespHeaders, "0")),
    SC =:= 200 andalso ContentLength =:= size(Body) andalso RespBody =:= Body;
http_check_resp(SC, RespHeaders, RespBody, Method, _) ->
    io:format(user, "[badmatch] sc:~p headers:~p body:~p method:~p ~n",[SC, RespHeaders, RespBody, Method]),
    false.

%% @doc inner functions
url_gen(Bucket, Path) when length(Bucket) > 0 andalso length(Path) > 0 ->
    lists:append(["http://",
                  ?TARGET_HOST,":",
                  ?TARGET_PORT,"/",
                  Bucket,"/",
                  Path]);
url_gen(Bucket, _Path) when length(Bucket) > 0 ->
    lists:append(["http://",
                  ?TARGET_HOST,":",
                  ?TARGET_PORT,"/",
                  Bucket,"/"]);
url_gen(_Bucket, _Path) ->
    lists:append(["http://",
                  ?TARGET_HOST,":",
                  ?TARGET_PORT,"/default/"]).

headers_gen() ->
    [{"connection", "close"},{"Authorization","auth"}].

raw_resp_gen('head', Bucket, Path, 'ok', Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {ok, #?METADATA{
             del = 0,
             timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
             checksum = 0,
             dsize = size(Body)}};
raw_resp_gen('head', _, _, 'ok', _) ->
    ok;
raw_resp_gen('head', Bucket, Path, 'not_found', _Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {error, not_found};
raw_resp_gen('head', _, _, 'not_found', _) ->
    not_found;
raw_resp_gen('head', _, _, 'timeout', _Body) ->
    {error, timeout};
raw_resp_gen('head', _, _, 'error', _Body) ->
    {error, ?ERR_TYPE_INTERNAL_ERROR};

raw_resp_gen('get', Bucket, Path, 'ok', Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {ok, #?METADATA{
             del = 0,
             timestamp = calendar:datetime_to_gregorian_seconds(calendar:universal_time()),
             checksum = 0,
             dsize = size(Body)}, Body};
raw_resp_gen('get', _, _, 'ok', Body) ->
    {ok, [], Body};
raw_resp_gen('get', _, _, 'not_found', _Body) ->
    {error, not_found};
raw_resp_gen('get', _, _, 'timeout', _Body) ->
    {error, timeout};
raw_resp_gen('get', _, _, 'error', _Body) ->
    {error, ?ERR_TYPE_INTERNAL_ERROR};

raw_resp_gen('put', Bucket, Path, 'ok', _Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {ok, 1};
raw_resp_gen('put', Bucket, Path, 'not_found', _Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {ok, 1};
raw_resp_gen('put', _, _, 'ok', _Body) ->
    ok;
raw_resp_gen('put', _, _, 'not_found', _Body) ->
    ok;
raw_resp_gen('put', _, _, 'timeout', _Body) ->
    {error, timeout};
raw_resp_gen('put', _, _, 'error', _Body) ->
    {error, ?ERR_TYPE_INTERNAL_ERROR};

raw_resp_gen('delete', _, _, 'ok', _Body) ->
    ok;
raw_resp_gen('delete', Bucket, Path, 'not_found', _Body) when length(Bucket) > 0 andalso length(Path) > 0 ->
    {error, not_found};
raw_resp_gen('delete', _, _, 'not_found', _Body) ->
    not_found;
raw_resp_gen('delete', _, _, 'timeout', _Body) ->
    {error, timeout};
raw_resp_gen('delete', _, _, 'error', _Body) ->
    {error, ?ERR_TYPE_INTERNAL_ERROR}.

