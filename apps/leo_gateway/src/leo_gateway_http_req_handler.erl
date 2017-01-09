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
%% Leo Gateway - HTTP Request Handler
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_http_req_handler).

-include("leo_http.hrl").
-include_lib("leo_dcerl/include/leo_dcerl.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([handle/4]).

%%--------------------------------------------------------------------
%% INVALID OPERATION
%%--------------------------------------------------------------------
%% @doc Constraint violation.
handle(_HTTPMethod, Req, Key, #req_params{token_length = Len,
                                          max_layers   = Max}) when Len > Max ->
    ?reply_not_found([?SERVER_HEADER], Key, <<>>, Req);

%% ---------------------------------------------------------------------
%% For BUCKET-OPERATION
%% ---------------------------------------------------------------------
%% @doc GET operation on buckets & Dirs.
handle(?HTTP_GET, Req, Key, #req_params{is_dir = true,
                                        handler = Handler} = Params) ->
    Handler:get_bucket(Req, Key, Params);

%% @doc PUT operation on buckets.
handle(?HTTP_PUT, Req, Key, #req_params{is_dir = true,
                                        token_length = 1,
                                        is_multi_delete = true,
                                        handler = Handler} = Params) ->
    Handler:put_object(Req, Key, Params);

%% @doc PUT operation on buckets.
handle(?HTTP_PUT, Req, Key, #req_params{is_dir = true,
                                        token_length = 1,
                                        handler = Handler} = Params) ->
    Handler:put_bucket(Req, Key, Params);

%% @doc DELETE operation on buckets.
%% @private
handle(?HTTP_DELETE, Req, Key, #req_params{is_dir = true,
                                           token_length = 1,
                                           handler = Handler} = Params) ->
    Handler:delete_bucket(Req, Key, Params);

%% @doc HEAD operation on buckets.
%% @private
handle(?HTTP_HEAD, Req, Key, #req_params{is_dir = true,
                                         token_length = 1,
                                         handler = Handler} = Params) ->
    Handler:head_bucket(Req, Key, Params);

%% ---------------------------------------------------------------------
%% For OBJECT-OPERATION
%% ---------------------------------------------------------------------
%% @doc GET operation on Object with Range Header.
handle(?HTTP_GET, Req, Key, #req_params{range_header = RangeHeader,
                                        handler = Handler} = Params) when RangeHeader /= undefined ->
    Handler:range_object(Req, Key, Params);

%% @doc GET operation on Object if inner cache is enabled.
%% @private
handle(?HTTP_GET = HTTPMethod, Req, Key, #req_params{is_cached = true,
                                                     has_inner_cache = true,
                                                     handler = Handler} = Params) ->
    case catch leo_cache_api:get_filepath(Key) of
        {ok, CacheMeta} when CacheMeta#cache_meta.file_path /= [] ->
            CachedObj = #cache{
                           etag         = CacheMeta#cache_meta.md5,
                           mtime        = CacheMeta#cache_meta.mtime,
                           content_type = CacheMeta#cache_meta.content_type,
                           body         = <<>>,
                           size         = CacheMeta#cache_meta.size,
                           file_path    = CacheMeta#cache_meta.file_path
                          },
            Handler:get_object_with_cache(Req, Key, CachedObj, Params);
        _ ->
            case catch leo_cache_api:get(Key) of
                {ok, CachedObj0} ->
                    CachedObj1 = binary_to_term(CachedObj0),
                    Handler:get_object_with_cache(Req, Key, CachedObj1, Params);
                _ ->
                    handle(HTTPMethod, Req, Key, Params#req_params{is_cached = false})
            end
    end;

%% @doc GET operation on Object.
%% @private
handle(?HTTP_GET, Req, Key, #req_params{handler = Handler} = Params) ->
    Handler:get_object(Req, Key, Params);

%% @doc POST/PUT operation on Objects.
%% @private
handle(?HTTP_PUT, Req, Key, #req_params{handler = Handler} = Params) ->
    Handler:put_object(Req, Key, Params);

%% @doc DELETE operation on Object.
%% @private
handle(?HTTP_DELETE, Req, Key, #req_params{handler = Handler} = Params) ->
    Handler:delete_object(Req, Key, Params);

%% @doc HEAD operation on Object.
%% @private
handle(?HTTP_HEAD, Req, Key, #req_params{handler = Handler} = Params) ->
    Handler:head_object(Req, Key, Params);

%% @doc invalid request.
%% @private
handle(_, Req, Key, _) ->
    ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidArgument, ?XML_ERROR_MSG_InvalidArgument, Key, <<>>, Req).

