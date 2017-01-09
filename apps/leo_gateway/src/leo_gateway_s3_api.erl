%%======================================================================
%%
%% Leo S3 Handler
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
%% Leo Gateway S3-API
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_s3_api).

-behaviour(leo_gateway_http_behaviour).

-export([start/2, stop/0,
         init/3, handle/2, terminate/3]).
-export([onrequest/1, onresponse/1]).
-export([get_bucket/3, put_bucket/3, delete_bucket/3, head_bucket/3,
         get_object/3, put_object/3, delete_object/3, head_object/3,
         get_object_with_cache/4, range_object/3
        ]).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_auth.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_s3_libs/include/leo_s3_endpoint.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-compile({inline, [handle/2, handle_1/4, handle_2/6,
                   handle_multi_upload_1/8,
                   handle_multi_upload_2/6,
                   handle_multi_upload_3/3,
                   gen_upload_key/1, gen_upload_initiate_xml/3, gen_upload_completion_xml/4,
                   resp_copy_obj_xml/2, request_params/2, auth/5, auth/7, auth_1/7,
                   get_bucket_1/6, put_bucket_1/3, delete_bucket_1/2, head_bucket_1/2
                  ]}).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Start cowboy's listeners
-spec(start(Sup, HttpOptions) ->
             ok | {error, Cause} when Sup::module(),
                                      HttpOptions::[{atom(), any()}],
                                      Cause::any()).
start(Sup, HttpOptions) ->
    leo_gateway_http_commons:start(Sup, HttpOptions).


%% @doc Stop cowboy's listeners
-spec(stop() ->
             ok).
stop() ->
    cowboy:stop_listener(?MODULE),
    cowboy:stop_listener(list_to_atom(lists:append([?MODULE_STRING, "_ssl"]))),
    ok.


%% @doc Initializer
init({_Any, http}, Req, Opts) ->
    {ok, Req, Opts}.


%% @doc Handle a request
%% @callback
-spec(handle(Req, State) ->
             {ok, Req, State} when Req::cowboy_req:req(),
                                   State::term()).
handle(Req, State) ->
    case leo_watchdog_state:find_not_safe_items() of
        not_found ->
            {Host,    _} = cowboy_req:host(Req),
            %% Host header must be included even if a request with HTTP/1.0
            case Host of
                <<>> ->
                    {ok, Req2} = ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidArgument,
                                                    ?XML_ERROR_MSG_InvalidArgument, <<>>, <<>>, Req),
                    {ok, Req2, State};
                _ ->
                    case check_request(Req) of
                        ok ->
                            {Bucket, Path} = get_bucket_and_path(Req),
                            handle_1(Req, State, Bucket, Path);
                        {error, Req2} ->
                            {ok, Req2, State}
                    end
            end;
        {ok, ErrorItems} ->
            ?debug("handle/2", "error-items:~p", [ErrorItems]),
            {ok, Req2} = ?reply_service_unavailable_error([?SERVER_HEADER], <<>>, <<>>, Req),
            {ok, Req2, State}
    end.


%% @doc Terminater
terminate(_Reason, _Req, _State) ->
    ok.


%% @doc Check whether request is valid or not
%% @private
-spec(check_request(Req) ->
             ok | {error, Cause} when Req::cowboy_req:req(),
                                      Cause::any()).
check_request(Req) ->
    CheckList = [
                 fun check_bad_date/1
                ],
    check_request(Req, CheckList).

%% @private
check_request(_Req, []) ->
    ok;
check_request(Req, [CheckFun|Rest]) ->
    case CheckFun(Req) of
        {error, 400, Code, Msg} ->
            {ok, Req2} = ?reply_bad_request([?SERVER_HEADER], Code, Msg, <<>>, <<>>, Req),
            {error, Req2};
        {error, 403, Code, Msg} ->
            {ok, Req2} = ?reply_forbidden([?SERVER_HEADER], Code, Msg, <<>>, <<>>, Req),
            {error, Req2};
        _ ->
            check_request(Req, Rest)
    end.

%% @private
check_bad_date(Req) ->
    case cowboy_req:header(?HTTP_HEAD_AUTHORIZATION, Req) of
        {undefined, _} ->
            %% no date header needed
            ok;
        _ ->
            check_bad_date_1(Req)
    end.

%% @private
check_bad_date_1(Req) ->
    case cowboy_req:header(?HTTP_HEAD_DATE, Req) of
        {undefined, _} ->
            case cowboy_req:header(?HTTP_HRAD_X_AMZ_DATE, Req) of
                {undefined, _} ->
                    {error, 403, ?XML_ERROR_CODE_AccessDenied, ?XML_ERROR_MSG_AccessDenied};
                {Date, _} ->
                    check_bad_date_invalid(Date)
            end;
        {Date, _} ->
            check_bad_date_invalid(Date)
    end.

%% @private
check_bad_date_invalid(Date) ->
    case catch cowboy_date:parse_date(Date) of
        {error, badarg} ->
            {error, 403, ?XML_ERROR_CODE_AccessDenied, ?XML_ERROR_MSG_AccessDenied};
        {'EXIT', _} ->
            {error, 403, ?XML_ERROR_CODE_AccessDenied, ?XML_ERROR_MSG_AccessDenied};
        {{Y,_,_},_}  ->
            case (Y =< 2010 orelse 2030 =< Y) of
                true ->
                    {error, 403, ?XML_ERROR_CODE_RequestTimeTooSkewed,
                     ?XML_ERROR_MSG_RequestTimeTooSkewed};
                _ ->
                    ok
            end
    end.


%%--------------------------------------------------------------------
%% Callbacks from Cowboy
%%--------------------------------------------------------------------
%% @doc Handle request
%%
-spec(onrequest(CacheCondition) ->
             Ret when CacheCondition::#cache_condition{},
                      Ret::any()).
onrequest(CacheCondition) ->
    leo_gateway_http_commons:onrequest(CacheCondition, fun get_bucket_and_path/1).


%% @doc Handle response
%%
-spec(onresponse(CacheCondition) ->
             Ret when CacheCondition::#cache_condition{},
                      Ret::any()).
onresponse(CacheCondition) ->
    leo_gateway_http_commons:onresponse(CacheCondition, fun get_bucket_and_path/1).


%% ---------------------------------------------------------------------
%% Callbacks from HTTP-Handler
%%
%% For BUCKET-OPERATION
%% ---------------------------------------------------------------------
%% @doc GET buckets and dirs
-spec(get_bucket(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
get_bucket(Req, Key, #req_params{access_key_id = AccessKeyId,
                                 is_acl = false,
                                 qs_prefix = Prefix}) ->
    BeginTime = leo_date:clock(),
    NormalizedMarker = case cowboy_req:qs_val(?HTTP_QS_BIN_MARKER, Req) of
                           {undefined,_} ->
                               <<>>;
                           {Marker,_} ->
                               %% Normalize Marker
                               %% Append $BucketName/ at the beginning of Marker as necessary
                               KeySize = size(Key),
                               case binary:match(Marker, Key) of
                                   {0, KeySize} ->
                                       Marker;
                                   _Other ->
                                       << Key/binary, Marker/binary >>
                               end
                       end,
    MaxKeys = case cowboy_req:qs_val(?HTTP_QS_BIN_MAXKEYS, Req) of
                  {undefined, _} ->
                      ?DEF_S3API_MAX_KEYS;
                  {Val_2,     _} ->
                      try
                          MaxKeys1 = binary_to_integer(Val_2),
                          erlang:min(MaxKeys1, ?HTTP_MAXKEYS_LIMIT)
                      catch _:_ ->
                              ?DEF_S3API_MAX_KEYS
                      end
              end,
    Delimiter = case cowboy_req:qs_val(?HTTP_QS_BIN_DELIMITER, Req) of
                    {undefined, _} -> none;
                    {Val, _} ->
                        Val
                end,

    PrefixBin = case Prefix of
                    none ->
                        <<>>;
                    _ ->
                        Prefix
                end,

    case get_bucket_1(AccessKeyId, Key, Delimiter, NormalizedMarker, MaxKeys, Prefix) of
        {ok, XMLRet} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_OK, BeginTime),
            Header = [?SERVER_HEADER,
                      {?HTTP_HEAD_RESP_CONTENT_TYPE, ?HTTP_CTYPE_XML}],
            ?reply_ok(Header, XMLRet, Req);
        {error, badarg} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_BAD_REQ, BeginTime),
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidArgument,
                               ?XML_ERROR_MSG_InvalidArgument, Key, <<>>, Req);
        {error, not_found} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_NOT_FOUND, BeginTime),
            ?reply_not_found([?SERVER_HEADER], Key, <<>>, Req);
        {error, unavailable} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_SERVICE_UNAVAILABLE, BeginTime),
            ?reply_service_unavailable_error([?SERVER_HEADER], Key, <<>>, Req);
        {error, ?ERR_TYPE_INTERNAL_ERROR} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_INTERNAL_ERROR, BeginTime),
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req);
        {error, timeout} ->
            ?access_log_bucket_get(Key, PrefixBin, ?HTTP_ST_SERVICE_UNAVAILABLE, BeginTime),
            ?reply_timeout([?SERVER_HEADER], Key, <<>>, Req)
    end;
get_bucket(Req, Bucket, #req_params{access_key_id = _AccessKeyId,
                                    is_acl = true}) ->
    Bucket_2 = formalize_bucket(Bucket),
    case leo_s3_bucket:find_bucket_by_name(Bucket_2) of
        {ok, BucketInfo} ->
            XML = generate_acl_xml(BucketInfo),
            Header = [?SERVER_HEADER,
                      {?HTTP_HEAD_RESP_CONTENT_TYPE, ?HTTP_CTYPE_XML}],
            ?reply_ok(Header, XML, Req);
        not_found ->
            ?reply_not_found([?SERVER_HEADER], Bucket_2, <<>>, Req);
        {error, _Cause} ->
            ?reply_internal_error([?SERVER_HEADER], Bucket_2, <<>>, Req)
    end.


%% @doc Put a bucket
-spec(put_bucket(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
put_bucket(Req, Key, #req_params{access_key_id = AccessKeyId,
                                 is_acl = false}) ->
    BeginTime = leo_date:clock(),
    Bucket = formalize_bucket(Key),
    CannedACL = string:to_lower(binary_to_list(?http_header(Req, ?HTTP_HEAD_X_AMZ_ACL))),
    %% Consume CreateBucketConfiguration
    Req_1 = case cowboy_req:has_body(Req) of
                false ->
                    Req;
                true ->
                    {ok, _Bin_2, Req_2} = cowboy_req:body(Req),
                    Req_2
            end,
    case put_bucket_1(CannedACL, AccessKeyId, Bucket) of
        ok ->
            ?access_log_bucket_put(Bucket, ?HTTP_ST_OK, BeginTime),
            ?reply_ok([?SERVER_HEADER], Req_1);
        {error, ?ERR_TYPE_INTERNAL_ERROR} ->
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req_1);
        {error, invalid_bucket_format} ->
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidBucketName,
                               ?XML_ERROR_MSG_InvalidBucketName, Key, <<>>, Req_1);
        {error, invalid_access} ->
            ?reply_forbidden([?SERVER_HEADER], ?XML_ERROR_CODE_AccessDenied,
                             ?XML_ERROR_MSG_AccessDenied, Key, <<>>, Req);
        {error, already_exists} ->
            ?reply_conflict([?SERVER_HEADER], ?XML_ERROR_CODE_BucketAlreadyExists,
                            ?XML_ERROR_MSG_BucketAlreadyExists, Key, <<>>, Req_1);
        {error, already_yours} ->
            ?reply_conflict([?SERVER_HEADER], ?XML_ERROR_CODE_BucketAlreadyOwnedByYou,
                            ?XML_ERROR_MSG_BucketAlreadyOwnedByYou, Key, <<>>, Req_1);
        {error, timeout} ->
            ?reply_timeout([?SERVER_HEADER], Key, <<>>, Req_1)
    end;
put_bucket(Req, Key, #req_params{access_key_id = AccessKeyId,
                                 is_acl = true}) ->
    Bucket = formalize_bucket(Key),
    CannedACL = string:to_lower(binary_to_list(?http_header(Req, ?HTTP_HEAD_X_AMZ_ACL))),
    case put_bucket_acl_1(CannedACL, AccessKeyId, Bucket) of
        ok ->
            ?reply_ok([?SERVER_HEADER], Req);
        {error, not_supported} ->
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidArgument,
                               ?XML_ERROR_MSG_InvalidArgument, Key, <<>>, Req);
        {error, invalid_access} ->
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_AccessDenied,
                               ?XML_ERROR_MSG_AccessDenied, Key, <<>>, Req);
        {error, _} ->
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req)
    end.


%% @doc Remove a bucket
-spec(delete_bucket(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
delete_bucket(Req, Key, #req_params{access_key_id = AccessKeyId}) ->
    BeginTime = leo_date:clock(),
    Bucket = formalize_bucket(Key),
    case delete_bucket_1(AccessKeyId, Key) of
        ok ->
            ?access_log_bucket_delete(Bucket, ?HTTP_ST_NO_CONTENT, BeginTime),
            ?reply_no_content([?SERVER_HEADER], Req);
        not_found ->
            ?access_log_bucket_delete(Bucket, ?HTTP_ST_NOT_FOUND, BeginTime),
            ?reply_not_found([?SERVER_HEADER], Key, <<>>, Req);
        {error, timeout} ->
            ?access_log_bucket_delete(Bucket, ?HTTP_ST_SERVICE_UNAVAILABLE, BeginTime),
            ?reply_timeout_without_body([?SERVER_HEADER], Req);
        {error, _} ->
            ?access_log_bucket_delete(Bucket, ?HTTP_ST_INTERNAL_ERROR, BeginTime),
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req)
    end.


%% @doc Retrieve a bucket-info
-spec(head_bucket(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
head_bucket(Req, Key, #req_params{access_key_id = AccessKeyId}) ->
    BeginTime = leo_date:clock(),
    Bucket = formalize_bucket(Key),
    case head_bucket_1(AccessKeyId, Bucket) of
        ok ->
            ?access_log_bucket_head(Bucket, ?HTTP_ST_OK, BeginTime),
            ?reply_ok([?SERVER_HEADER], Req);
        not_found ->
            ?access_log_bucket_head(Bucket, ?HTTP_ST_NOT_FOUND, BeginTime),
            ?reply_not_found_without_body([?SERVER_HEADER], Req);
        {error, timeout} ->
            ?access_log_bucket_head(Bucket, ?HTTP_ST_SERVICE_UNAVAILABLE, BeginTime),
            ?reply_timeout_without_body([?SERVER_HEADER], Req);
        {error, _} ->
            ?access_log_bucket_delete(Bucket, ?HTTP_ST_INTERNAL_ERROR, BeginTime),
            ?reply_internal_error_without_body([?SERVER_HEADER], Req)
    end.


%% ---------------------------------------------------------------------
%% For OBJECT-OPERATION
%% ---------------------------------------------------------------------
%% @doc GET operation on Objects
-spec(get_object(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
get_object(Req, Key, Params) ->
    leo_gateway_http_commons:get_object(Req, Key, Params).


%% @doc GET operation on Objects
-spec(get_object_with_cache(Req, Key, CacheObj, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            CacheObj::#cache{},
                            ReqParams::#req_params{}).
get_object_with_cache(Req, Key, CacheObj, Params) ->
    leo_gateway_http_commons:get_object_with_cache(Req, Key, CacheObj,  Params).


%% @doc utility func for getting x-amz-meta-directive correctly
-spec(get_x_amz_meta_directive(Req) ->
             Ret when Req::cowboy_req:req(),
                      Ret::binary()).
get_x_amz_meta_directive(Req) ->
    Directive = ?http_header(Req, ?HTTP_HEAD_X_AMZ_META_DIRECTIVE),
    get_x_amz_meta_directive(Req, Directive).

%% @private
get_x_amz_meta_directive(Req, ?BIN_EMPTY) ->
    CS = ?http_header(Req, ?HTTP_HEAD_X_AMZ_COPY_SOURCE),
    case CS of
        ?BIN_EMPTY ->
            ?BIN_EMPTY;
        _ ->
            %% return default - 'copy'
            ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_COPY
    end;
get_x_amz_meta_directive(_Req, Other) ->
    Other.


%% @doc POST/PUT operation on Objects
-spec(put_object(Req, Key, ReqParams) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
put_object(Req, Key, Params) ->
    put_object(get_x_amz_meta_directive(Req), Req, Key, Params).

%% @doc handle MULTIPLE DELETE request
-spec(put_object(Directive, Req, Key, ReqParams) ->
             {ok, Req} when Directive::binary(),
                            Req::cowboy_req:req(),
                            Key::binary(),
                            ReqParams::#req_params{}).
put_object(?BIN_EMPTY, Req, _Key, #req_params{is_multi_delete = true,
                                              timeout_for_body = Timeout4Body,
                                              transfer_decode_fun = TransferDecodeFun,
                                              transfer_decode_state = TransferDecodeState} = Params) ->
    BodyOpts = case TransferDecodeFun of
                   undefined ->
                       [{read_timeout, Timeout4Body}];
                   _ ->
                       [{read_timeout, Timeout4Body},
                        {transfer_decode, TransferDecodeFun, TransferDecodeState}]
               end,
    case cowboy_req:body(Req, BodyOpts) of
        {ok, Body, Req1} ->
            %% Check Content-MD5 with body
            ContentMD5 = ?http_header(Req, ?HTTP_HEAD_CONTENT_MD5),
            CalculatedMD5 = base64:encode(crypto:hash(md5, Body)),
            delete_multi_objects_2(Req1, Body, ContentMD5, CalculatedMD5, Params);
        {error, _Cause} ->
            ?reply_malformed_xml([?SERVER_HEADER], Req)
    end;

put_object(?BIN_EMPTY, Req, Key, Params) ->
    case catch cowboy_req:body_length(Req) of
        {'EXIT', _} ->
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidArgument,
                               ?XML_ERROR_MSG_InvalidArgument, Key, <<>>, Req);
        {BodySize, _} ->
            Size = case cowboy_req:header(?HTTP_HEAD_X_AMZ_DECODED_CONTENT_LENGTH, Req) of
                       {undefined,_} ->
                           BodySize;
                       {Val,_} ->
                           binary_to_integer(Val)
                   end,
            case (Size >= Params#req_params.threshold_of_chunk_len) of
                true when Size >= Params#req_params.max_len_of_obj ->
                    ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_EntityTooLarge,
                                       ?XML_ERROR_MSG_EntityTooLarge, Key, <<>>, Req);
                true when Params#req_params.is_upload == false ->
                    leo_gateway_http_commons:put_large_object(Req, Key, Size, Params);
                false ->
                    Ret = case cowboy_req:has_body(Req) of
                              true ->
                                  TransferDecodeFun = Params#req_params.transfer_decode_fun,
                                  TransferDecodeState = Params#req_params.transfer_decode_state,
                                  Timeout4Body = Params#req_params.timeout_for_body,
                                  BodyOpts = case TransferDecodeFun of
                                                 undefined ->
                                                     [{read_timeout, Timeout4Body}];
                                                 _ ->
                                                     [{read_timeout, Timeout4Body},
                                                      {transfer_decode, TransferDecodeFun, TransferDecodeState}]
                                             end,
                                  case cowboy_req:body(Req, BodyOpts) of
                                      {ok, Bin, Req1} ->
                                          {ok, {Size, Bin, Req1}};
                                      {error, Cause} ->
                                          {error, Cause}
                                  end;
                              false ->
                                  {ok, {0, ?BIN_EMPTY, Req}}
                          end,
                    leo_gateway_http_commons:put_small_object(Ret, Key, Params)
            end
    end;

%% @doc POST/PUT operation on Objects. COPY/REPLACE
%% @private
put_object(Directive, Req, Key, #req_params{handler = ?PROTO_HANDLER_S3,
                                            custom_metadata = CMetaBin1} = Params) ->
    CS = cow_qs:urldecode(?http_header(Req, ?HTTP_HEAD_X_AMZ_COPY_SOURCE)),

    %% need to trim head '/' when cooperating with s3fs(-c)
    CS2 = case binary:part(CS, {0, 1}) of
              ?BIN_SLASH ->
                  binary:part(CS, {1, byte_size(CS) -1});
              _ ->
                  CS
          end,

    case (Key =:= CS2) of
        true ->
            %% 400
            ?reply_bad_request([?SERVER_HEADER], ?XML_ERROR_CODE_InvalidRequest,
                               ?XML_ERROR_MSG_InvalidRequest, Key, <<>>, Req);
        false ->
            case leo_gateway_rpc_handler:get(CS2) of
                {ok, Meta, RespObject} ->
                    CMetaBin = case Directive of
                                   ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_COPY ->
                                       Meta#?METADATA.meta;
                                   _ ->
                                       CMetaBin1
                               end,
                    case Meta#?METADATA.cnumber of
                        0 ->
                            put_object_1(Directive, Req, Key, Meta, RespObject, Params#req_params{custom_metadata = CMetaBin});
                        _TotalChunkedObjs ->
                            put_large_object_1(Directive, Req, Key, Meta, Params#req_params{custom_metadata = CMetaBin})
                    end;
                {error, not_found} ->
                    ?reply_not_found([?SERVER_HEADER], Key, <<>>, Req);
                {error, unavailable} ->
                    ?reply_service_unavailable_error([?SERVER_HEADER], Key, <<>>, Req);
                {error, ?ERR_TYPE_INTERNAL_ERROR} ->
                    ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req);
                {error, timeout} ->
                    ?reply_timeout([?SERVER_HEADER], Key, <<>>, Req)
            end
    end.

%% @doc POST/PUT operation on Objects. COPY
%% @private
put_object_1(Directive, Req, Key, Meta, Bin, #req_params{bucket_name = BucketName,
                                                         bucket_info = BucketInfo,
                                                         custom_metadata = CMetaBin} = Params) ->
    BeginTime = leo_date:clock(),
    Size = size(Bin),
    case leo_gateway_rpc_handler:put(#put_req_params{path = Key,
                                                     body = Bin,
                                                     meta = CMetaBin,
                                                     dsize = Size,
                                                     msize = byte_size(CMetaBin),
                                                     bucket_info = BucketInfo}) of
        {ok, _ETag} when Directive == ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_COPY ->
            ?access_log_put(BucketName, Key, Size, ?HTTP_ST_OK, BeginTime),
            resp_copy_obj_xml(Req, Meta);
        {ok, _ETag} when Directive == ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_REPLACE ->
            put_object_2(Req, Key, Meta, Params);
        {error, unavailable} ->
            ?reply_service_unavailable_error([?SERVER_HEADER], Key, <<>>, Req);
        {error, ?ERR_TYPE_INTERNAL_ERROR} ->
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req);
        {error, timeout} ->
            ?reply_timeout([?SERVER_HEADER], Key, <<>>, Req)
    end.

%% @doc POST/PUT operation on Objects. REPLACE
%% @private
put_object_2(Req, Key, Meta, Params) ->
    case Key == Meta#?METADATA.key of
        true ->
            resp_copy_obj_xml(Req, Meta);
        false ->
            put_object_3(Req, Meta, Params)
    end.

%% @private
put_object_3(Req, #?METADATA{key = Key, dsize = Size} = Meta, #req_params{bucket_name = BucketName}) ->
    BeginTime = leo_date:clock(),
    case leo_gateway_rpc_handler:delete(Meta#?METADATA.key) of
        ok ->
            ?access_log_delete(BucketName, Key, Size, ?HTTP_ST_NO_CONTENT, BeginTime),
            resp_copy_obj_xml(Req, Meta);
        {error, not_found} ->
            resp_copy_obj_xml(Req, Meta);
        {error, unavailable} ->
            ?reply_service_unavailable_error([?SERVER_HEADER], Meta#?METADATA.key, <<>>, Req);
        {error, ?ERR_TYPE_INTERNAL_ERROR} ->
            ?reply_internal_error([?SERVER_HEADER], Meta#?METADATA.key, <<>>, Req);
        {error, timeout} ->
            ?reply_timeout([?SERVER_HEADER], Meta#?METADATA.key, <<>>, Req)
    end.

%% @doc POST/PUT operation on `Large` Objects. COPY
%% @private
put_large_object_1(Directive, Req, Key, Meta, Params) ->
    case leo_gateway_http_commons:move_large_object(Meta, Key, Params) of
        ok when Directive == ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_COPY ->
            resp_copy_obj_xml(Req, Meta);
        ok when Directive == ?HTTP_HEAD_X_AMZ_META_DIRECTIVE_REPLACE ->
            put_large_object_2(Req, Key, Meta);
        {error, timeout} ->
            ?reply_timeout([?SERVER_HEADER], Key, <<>>, Req);
        {error, _Other} ->
            ?reply_internal_error([?SERVER_HEADER], Key, <<>>, Req)
    end.

%% @doc POST/PUT operation on Objects. REPLACE
%% @private
put_large_object_2(Req, Key, Meta) ->
    case Key == Meta#?METADATA.key of
        true ->
            resp_copy_obj_xml(Req, Meta);
        false ->
            put_large_object_3(Req, Meta)
    end.

%% @private
put_large_object_3(Req, Meta) ->
    leo_large_object_commons:delete_chunked_objects(Meta#?METADATA.key),
    catch leo_gateway_rpc_handler:delete(Meta#?METADATA.key),
    resp_copy_obj_xml(Req, Meta).


%% @doc DELETE operation on Objects
-spec(delete_object(cowboy_req:req(), binary(), #req_params{}) ->
             {ok, cowboy_req:req()}).
delete_object(Req, Key, Params) ->
    leo_gateway_http_commons:delete_object(Req, Key, Params).


%% @doc HEAD operation on Objects
-spec(head_object(cowboy_req:req(), binary(), #req_params{}) ->
             {ok, cowboy_req:req()}).
head_object(Req, Key, Params) ->
    leo_gateway_http_commons:head_object(Req, Key, Params).


%% @doc RANGE-Query operation on Objects
-spec(range_object(cowboy_req:req(), binary(), #req_params{}) ->
             {ok, cowboy_req:req()}).
range_object(Req, Key, Params) ->
    leo_gateway_http_commons:range_object(Req, Key, Params).


%% ---------------------------------------------------------------------
%% Inner Functions
%% ---------------------------------------------------------------------
%% @doc Create a key
%% @private
-spec(get_bucket_and_path(Req) ->
             {ok, Ret} when Req::cowboy_req:req(),
                            Ret::{binary(), binary()}).
get_bucket_and_path(Req) ->
    {RawPath, _} = cowboy_req:path(Req),
    Path = cow_qs:urldecode(RawPath),
    get_bucket_and_path(Req, Path).

%% @private
get_bucket_and_path(Req, Path) ->
    EndPoints_2 = case leo_s3_endpoint:get_endpoints() of
                      {ok, EndPoints_1} ->
                          [Ep || #endpoint{endpoint = Ep} <- EndPoints_1];
                      _ ->
                          []
                  end,
    {Host,_} = cowboy_req:host(Req),
    leo_http:key(EndPoints_2, Host, Path).


%% @doc Handle an http-request
%% @private
-spec(handle_1(Req, State, BucketName, Path) ->
             {ok, Req, State} when Req::cowboy_req:req(),
                                   State::[any()],
                                   BucketName::binary(),
                                   Path::binary()).
handle_1(Req, [{NumOfMinLayers, NumOfMaxLayers},
               HasInnerCache, CustomHeaderSettings, Props] = State, BucketName, Path) ->
    BinPart = binary:part(Path, {byte_size(Path)-1, 1}),
    TokenLen = length(binary:split(Path, [?BIN_SLASH], [global, trim])),
    HTTPMethod = cowboy_req:get(method, Req),

    {Prefix, IsDir, Path_1, Req_2} =
        case cowboy_req:qs_val(?HTTP_HEAD_PREFIX, Req) of
            {undefined, Req_1} ->
                {none, (TokenLen == 1 orelse ?BIN_SLASH == BinPart), Path, Req_1};
            {BinParam, Req_1} ->
                NewPath = case BinPart of
                              ?BIN_SLASH ->
                                  Path;
                              _ ->
                                  << Path/binary, ?BIN_SLASH/binary >>
                          end,
                {BinParam, true, NewPath, Req_1}
        end,

    IsACL = case cowboy_req:qs_val(?HTTP_QS_BIN_ACL, Req_2) of
                {undefined, _} ->
                    false;
                _ ->
                    true
            end,
    ReqParams = request_params(Req_2,
                               #req_params{
                                  handler = ?MODULE,
                                  path = Path_1,
                                  bucket_name = BucketName,
                                  token_length = TokenLen,
                                  min_layers = NumOfMinLayers,
                                  max_layers = NumOfMaxLayers,
                                  qs_prefix = Prefix,
                                  has_inner_cache = HasInnerCache,
                                  is_cached = true,
                                  is_dir = IsDir,
                                  is_acl = IsACL,
                                  max_chunked_objs = Props#http_options.max_chunked_objs,
                                  max_len_of_obj = Props#http_options.max_len_of_obj,
                                  chunked_obj_len = Props#http_options.chunked_obj_len,
                                  custom_header_settings = CustomHeaderSettings,
                                  timeout_for_header = Props#http_options.timeout_for_header,
                                  timeout_for_body = Props#http_options.timeout_for_body,
                                  sending_chunked_obj_len = Props#http_options.sending_chunked_obj_len,
                                  reading_chunked_obj_len = Props#http_options.reading_chunked_obj_len,
                                  threshold_of_chunk_len = Props#http_options.threshold_of_chunk_len}),
    case ReqParams of
        {error, metadata_too_large} ->
            {ok, Req_3} = ?reply_metadata_too_large([?SERVER_HEADER], Path_1, <<>>, Req_2),
            {ok, Req_3, State};
        _ ->
            AuthRet = auth(Req_2, HTTPMethod, Path_1, TokenLen, ReqParams),
            AuthRet_2 = case AuthRet of
                            {error, Reason} ->
                                {error, Reason};
                            {ok, AccessKeyId, _} ->
                                {ok, AccessKeyId}
                        end,
            ReqParams_2 = case ReqParams#req_params.is_aws_chunked of
                              true ->
                                  case AuthRet of
                                      {ok, _, SignParams} ->
                                          {Signature, SignHead, SignKey} =
                                          case SignParams of
                                              undefined ->
                                                  {undefined, undefined, undefined};
                                              _ ->
                                                  SignParams
                                          end,
                                          AWSChunkSignParams = #aws_chunk_sign_params{
                                                                  sign_head = SignHead,
                                                                  sign_key = SignKey,
                                                                  prev_sign = Signature,
                                                                  chunk_sign = <<>>},
                                          AWSChunkDecState = #aws_chunk_decode_state{
                                                                buffer = <<>>,
                                                                dec_state = wait_size,
                                                                chunk_offset = 0,
                                                                sign_params = AWSChunkSignParams,
                                                                total_len = 0},
                                          ReqParams#req_params{
                                            transfer_decode_fun = fun aws_chunk_decode/2,
                                            transfer_decode_state = AWSChunkDecState};
                                      _ ->
                                          ReqParams
                                  end;
                              _ ->
                                  ReqParams
                          end,
            handle_2(AuthRet_2, Req_2, HTTPMethod, Path_1, ReqParams_2, State)
    end.


%% @doc Handle a request (sub)
%% @private
-spec(handle_2(Ret, Req, HttpVerb, Path, ReqParams, State) ->
             {ok, Req, State} when Ret::{ok, AccessKeyId} | {error, Cause},
                                   AccessKeyId::binary(),
                                   Cause::any(),
                                   Req::cowboy_req:req(),
                                   HttpVerb::binary(),
                                   Path::binary(),
                                   ReqParams::#req_params{},
                                   State::[any()]).
handle_2({error, unmatch}, Req,_HttpVerb, Key,_ReqParams, State) ->
    {ok, Req_2} = ?reply_forbidden([?SERVER_HEADER],
                                   ?XML_ERROR_CODE_SignatureDoesNotMatch,
                                   ?XML_ERROR_MSG_SignatureDoesNotMatch, Key, <<>>, Req),
    {ok, Req_2, State};
handle_2({error, not_found}, Req,_HttpVerb, Key,_ReqParams, State) ->
    {ok, Req_2} = ?reply_not_found([?SERVER_HEADER], Key, <<>>, Req),
    {ok, Req_2, State};
handle_2({error, already_yours}, Req,_HttpVerb, Key,_ReqParams, State) ->
    {ok, Req_2} = ?reply_conflict([?SERVER_HEADER], ?XML_ERROR_CODE_BucketAlreadyOwnedByYou,
                                  ?XML_ERROR_MSG_BucketAlreadyOwnedByYou, Key, <<>>, Req),
    {ok, Req_2, State};
handle_2({error, _Cause}, Req,_HttpVerb, Key,_ReqParams,State) ->
    {ok, Req_2} = ?reply_forbidden([?SERVER_HEADER],
                                   ?XML_ERROR_CODE_AccessDenied,
                                   ?XML_ERROR_MSG_AccessDenied, Key, <<>>, Req),
    {ok, Req_2, State};

%% For Multipart Upload - Initiation
handle_2({ok,_AccessKeyId}, Req, ?HTTP_POST,_Key, #req_params{bucket_info = BucketInfo,
                                                              custom_metadata = CMetaBin,
                                                              path = Path,
                                                              is_upload = true}, State) ->
    %% remove a registered object with 'touch-command'
    %% from the cache
    catch leo_cache_api:delete(Path),

    %% Insert a metadata into the storage-cluster
    NowBin = list_to_binary(integer_to_list(leo_date:now())),
    UploadId = leo_hex:binary_to_hex(
                 crypto:hash(md5, << Path/binary, NowBin/binary >>)),
    UploadIdBin = list_to_binary(UploadId),
    UploadKey = << Path/binary, ?STR_NEWLINE, UploadIdBin/binary >>,

    {ok, Req_2} =
        case leo_gateway_rpc_handler:put(#put_req_params{path = UploadKey,
                                                         body = ?BIN_EMPTY,
                                                         meta = CMetaBin,
                                                         dsize = 0,
                                                         msize = byte_size(CMetaBin),
                                                         bucket_info = BucketInfo}) of
            {ok, _ETag} ->
                %% Response xml to a client
                [BucketName|Path_1] = leo_misc:binary_tokens(Path, ?BIN_SLASH),
                XML = gen_upload_initiate_xml(BucketName, Path_1, UploadId),
                ?reply_ok([?SERVER_HEADER], XML, Req);
            {error, unavailable} ->
                ?reply_service_unavailable_error([?SERVER_HEADER], Path, <<>>, Req);
            {error, timeout} ->
                ?reply_timeout([?SERVER_HEADER], Path, <<>>, Req);
            {error, Cause} ->
                ?error("handle_2/6", [{key, binary_to_list(Path)}, {cause, Cause}]),
                ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req)
        end,
    {ok, Req_2, State};

%% For Multipart Upload - Upload a part of an object
%% @private
handle_2({ok,_AccessKeyId}, Req, ?HTTP_PUT, Key,
         #req_params{upload_id = UploadId,
                     upload_part_num = PartNum,
                     max_chunked_objs = MaxChunkedObjs}, State) when UploadId /= <<>>,
                                                                     PartNum > MaxChunkedObjs ->
    {ok, Req_2} = ?reply_bad_request([?SERVER_HEADER],
                                     ?XML_ERROR_CODE_EntityTooLarge,
                                     ?XML_ERROR_MSG_EntityTooLarge,
                                     Key, <<>>, Req),
    {ok, Req_2, State};

handle_2({ok,_AccessKeyId}, Req, ?HTTP_PUT,_Key,
         #req_params{path = Path,
                     is_upload = false,
                     upload_id = UploadId,
                     upload_part_num = PartNum1} = Params, State) when UploadId /= <<>>,
                                                                       PartNum1 /= 0 ->
    PartNum2 = list_to_binary(integer_to_list(PartNum1)),
    %% for confirmation
    Key1 = << Path/binary, ?STR_NEWLINE, UploadId/binary >>,
    %% for put a part of an object
    Key2 = << Path/binary, ?STR_NEWLINE, PartNum2/binary >>,

    {ok, Req_2} =
        case leo_gateway_rpc_handler:head(Key1) of
            {ok, _Metadata} ->
                put_object(?BIN_EMPTY, Req, Key2, Params);
            {error, not_found} ->
                ?reply_not_found([?SERVER_HEADER], Path, <<>>, Req);
            {error, unavailable} ->
                ?reply_service_unavailable_error(
                   [?SERVER_HEADER], Path, <<>>, Req);
            {error, timeout} ->
                ?reply_timeout([?SERVER_HEADER], Path, <<>>, Req);
            {error, ?ERR_TYPE_INTERNAL_ERROR} ->
                ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req)
        end,
    {ok, Req_2, State};

handle_2({ok,_AccessKeyId}, Req, ?HTTP_DELETE,_Key,
         #req_params{bucket_info = BucketInfo,
                     path = Path,
                     upload_id = UploadId}, State) when UploadId /= <<>> ->
    _ = leo_gateway_rpc_handler:put(#put_req_params{path = Path,
                                                    body = ?BIN_EMPTY,
                                                    dsize = 0,
                                                    bucket_info = BucketInfo}),
    _ = leo_gateway_rpc_handler:delete(Path),
    _ = leo_gateway_rpc_handler:delete(<< Path/binary, ?STR_NEWLINE >>),
    {ok, Req_2} = ?reply_no_content([?SERVER_HEADER], Req),
    {ok, Req_2, State};

%% For Multipart Upload - Completion
handle_2({ok,_AccessKeyId}, Req, ?HTTP_POST,_Key,
         #req_params{bucket_info = BucketInfo,
                     path = Path,
                     chunked_obj_len = ChunkedLen,
                     is_upload = false,
                     upload_id = UploadId,
                     upload_part_num = PartNum,
                     transfer_decode_fun = TransferDecodeFun,
                     transfer_decode_state = TransferDecodeState}, State) when UploadId /= <<>>,
                                                                               PartNum == 0 ->
    Res = cowboy_req:has_body(Req),

    {ok, Req_2} = handle_multi_upload_1(
                    Res, Req, Path, UploadId,
                    ChunkedLen, TransferDecodeFun, TransferDecodeState, BucketInfo),
    {ok, Req_2, State};

%% For Regular cases
handle_2({ok, AccessKeyId}, Req, ?HTTP_POST,  Path, Params, State) ->
    handle_2({ok, AccessKeyId}, Req, ?HTTP_PUT,  Path, Params, State);

handle_2({ok, AccessKeyId}, Req, HTTPMethod, Path, Params, State) ->
    case catch leo_gateway_http_req_handler:handle(
                 HTTPMethod, Req,
                 Path, Params#req_params{access_key_id = AccessKeyId}) of
        {'EXIT', {"aws-chunked decode failed", _} = Cause} ->
            ?error("handle_2/6", [{key, binary_to_list(Path)},
                                  {cause, Cause}]),
            {ok, Req_2} = ?reply_forbidden(
                             [?SERVER_HEADER], ?XML_ERROR_CODE_AccessDenied,
                             ?XML_ERROR_MSG_AccessDenied, Path, <<>>, Req),
            {ok, Req_2, State};
        {'EXIT', Cause} ->
            ?error("handle_2/6", [{key, binary_to_list(Path)},
                                  {cause, Cause}]),
            {ok, Req_2} = ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req),
            {ok, Req_2, State};
        {ok, Req_2} ->
            Req_3 = cowboy_req:compact(Req_2),
            {ok, Req_3, State}
    end.


%% @private
-spec(aws_chunk_decode(Bin, State) ->
             {more|done, Acc, State} when Bin::binary(),
                                          State::#aws_chunk_decode_state{},
                                          Acc::binary()).
aws_chunk_decode(Bin, State) ->
    Buffer = State#aws_chunk_decode_state.buffer,
    DecState = State#aws_chunk_decode_state.dec_state,
    Offset = State#aws_chunk_decode_state.chunk_offset,
    SignParams = State#aws_chunk_decode_state.sign_params,
    TotalLen = State#aws_chunk_decode_state.total_len,
    Ret = aws_chunk_decode({ok, <<>>}, << Buffer/binary, Bin/binary >>,
                           DecState, Offset, SignParams),
    case Ret of
        {{error, Reason2}, {_, _, _, _}} ->
            ?error("aws_chunk_decode/2", [{simple_cause, "parsing error"},
                                          {cause, Reason2}]),
            erlang:error("aws-chunked decode failed");
        {{ok, Acc}, {Buffer_2, DecState_2, Offset_2, SignParams_2}} ->
            {more, Acc, #aws_chunk_decode_state{buffer = Buffer_2,
                                                dec_state = DecState_2,
                                                chunk_offset = Offset_2,
                                                sign_params = SignParams_2,
                                                total_len = TotalLen + byte_size(Acc)}};
        {{done, Acc}, {Rest, _, _, _}} ->
            {done, Acc, TotalLen + byte_size(Acc), Rest}
    end.

%% @private
aws_chunk_decode({ok, Acc}, Buffer, wait_size, 0,
                 #aws_chunk_sign_params{sign_head = SignHead} = SignParams) ->
    case byte_size(Buffer) of
        Len when Len > 10 ->
            << Bin:10/binary, _/binary >> = Buffer,
            case binary:match(Bin, <<";">>) of
                nomatch ->
                    {{error, incorrect}, {Buffer, error, 0, SignParams}};
                {Start, _} ->
                    << SizeHexBin:Start/binary, ";", Rest/binary >> = Buffer,
                    SizeHex = binary_to_list(SizeHexBin),
                    Size = leo_hex:hex_to_integer(SizeHex),
                    SignParams_2 =
                        case SignHead of
                            undefined ->
                                SignParams#aws_chunk_sign_params{chunk_size = Size};
                            _ ->
                                Context = crypto:hash_init(sha256),
                                SignParams#aws_chunk_sign_params{chunk_size = Size,
                                                                 hash_context = Context}
                        end,
                    aws_chunk_decode({ok, Acc}, Rest, wait_head, 0, SignParams_2)
            end;
        _ ->
            {{ok, Acc}, {Buffer, wait_size, 0, SignParams}}
    end;
aws_chunk_decode({ok, Acc}, Buffer, wait_head, 0, SignParams) ->
    case byte_size(Buffer) of
        Len when Len > 80 + 2 ->
            << "chunk-signature=", ChunkSign:64/binary,
               "\r\n", Rest/binary >> = Buffer,

            aws_chunk_decode({ok, Acc}, Rest, read_chunk, 0,
                             SignParams#aws_chunk_sign_params{chunk_sign = ChunkSign});
        _ ->
            {{ok, Acc}, {Buffer, wait_head, 0, SignParams}}
    end;
aws_chunk_decode({ok, Acc}, Buffer, read_chunk, Offset,
                 #aws_chunk_sign_params{sign_head = SignHead,
                                        sign_key = SignKey,
                                        prev_sign = PrevSign,
                                        chunk_sign = ChunkSign,
                                        chunk_size = ChunkSize,
                                        hash_context = Context} = SignParams) ->
    ChunkRemainSize = ChunkSize - Offset,
    case byte_size(Buffer) of
        Len when Len >= ChunkRemainSize + 2 ->
            << ChunkPart:ChunkRemainSize/binary,
               "\r\n", Rest/binary >> = Buffer,
            case SignHead of
                undefined ->
                    ?debug("aws_chunk_decode/4", "Output Chunk Size: ~p, No Sign", [ChunkSize]),
                    case ChunkSize of
                        %% Last Chunk
                        0 ->
                            {{done, Acc}, {Rest, done, 0, #aws_chunk_sign_params{}}};
                        _ ->
                            aws_chunk_decode({ok, << Acc/binary, ChunkPart/binary >>},
                                             Rest, wait_size, 0, SignParams)
                    end;
                _ ->
                    Context_2 = crypto:hash_update(Context, ChunkPart),
                    ChunkHash = crypto:hash_final(Context_2),
                    ChunkHashBin = leo_hex:binary_to_hexbin(ChunkHash),
                    BinToSign = << ?AWS_SIGNATURE_V4_SHA256_KEY/binary,
                                   "\n",
                                   SignHead/binary,
                                   PrevSign/binary,
                                   "\n",
                                   ?AWS_SIGNATURE_V4_SHA256_HASH/binary,
                                   "\n",
                                   ChunkHashBin/binary >>,

                    case (leo_hex:binary_to_hexbin(
                            crypto:hmac(sha256, SignKey, BinToSign))) of
                        ChunkSign ->
                            case (ChunkSize == 0) of
                                %% Last Chunk
                                true ->
                                    {{done, Acc}, {Rest, done, 0, #aws_chunk_sign_params{}}};
                                false ->
                                    ?debug("aws_chunk_decode/4",
                                           "Output Chunk Size: ~p, Sign: ~p", [ChunkSize, ChunkSign]),
                                    aws_chunk_decode({ok, << Acc/binary, ChunkPart/binary >>},
                                                     Rest, wait_size, 0,
                                                     SignParams#aws_chunk_sign_params{prev_sign = ChunkSign,
                                                                                      chunk_sign = <<>>})
                            end;
                        WrongSign ->
                            ?error("aws_chunk_decode/4",
                                   [{cause, "Chunk Signature Not Match"},
                                    {wrong_sign, WrongSign},
                                    {chunk_sign, ChunkSign},
                                    {sign, binary_to_list(BinToSign)}]),
                            {{error, unmatch}, {Buffer, error, Offset, SignParams}}
                    end
            end;
        Len when ChunkRemainSize >= Len ->
            SignParams_2 = case SignHead of
                               undefined ->
                                   SignParams;
                               _ ->
                                   Context_2 = crypto:hash_update(Context, Buffer),
                                   SignParams#aws_chunk_sign_params{hash_context = Context_2}
                           end,
            {{ok, << Acc/binary, Buffer/binary >>},
             {<<>>, read_chunk, Offset + Len, SignParams_2}};
        _ ->
            {{ok, Acc},
             {Buffer, read_chunk, Offset ,SignParams}}
    end.


%% @doc Handle multi-upload processing
%% @private
-spec(handle_multi_upload_1(IsHandling, Req, Path, UploadId,
                            ChunkedLen, TransferDecodeFun, TransferDecodeState, BucketInfo) ->
             {ok, Req} when IsHandling::boolean(),
                            Req::cowboy_req:req(),
                            Path::binary(),
                            UploadId::binary(),
                            ChunkedLen::non_neg_integer(),
                            TransferDecodeFun::function(),
                            TransferDecodeState::term(),
                            BucketInfo::#?BUCKET{}).
handle_multi_upload_1(true, Req, Path, UploadId,
                      ChunkedLen, TransferDecodeFun, TransferDecodeState, BucketInfo) ->
    Path4Conf = << Path/binary, ?STR_NEWLINE, UploadId/binary >>,

    case leo_gateway_rpc_handler:get(Path4Conf) of
        {ok, #?METADATA{meta = CMetaBin}, _} ->
            _ = leo_gateway_rpc_handler:delete(Path4Conf),

            BodyOpts = case TransferDecodeFun of
                           undefined ->
                               [];
                           _ ->
                               [{transfer_decode, TransferDecodeFun, TransferDecodeState}]
                       end,
            Ret = cowboy_req:body(Req, BodyOpts),
            handle_multi_upload_2(Ret, Req, Path, ChunkedLen, BucketInfo, CMetaBin);
        {error, unavailable} ->
            ?reply_service_unavailable_error([?SERVER_HEADER], Path, <<>>, Req);
        _ ->
            ?reply_forbidden([?SERVER_HEADER], ?XML_ERROR_CODE_AccessDenied,
                             ?XML_ERROR_MSG_AccessDenied, Path, <<>>, Req)
    end;
handle_multi_upload_1(false, Req, Path,_UploadId,_ChunkedLen,_,_,_) ->
    ?reply_forbidden([?SERVER_HEADER], ?XML_ERROR_CODE_AccessDenied,
                     ?XML_ERROR_MSG_AccessDenied, Path, <<>>, Req).

%% @private
-spec(handle_multi_upload_2({ok, Bin, Req}|{error, Cause}, Req, Path, ChunkedLen, BucketInfo, CMetaBin) ->
             {ok, Req} when Bin::binary(),
                            Req::cowboy_req:req(),
                            Cause::any(),
                            Path::binary(),
                            ChunkedLen::non_neg_integer(),
                            BucketInfo::#?BUCKET{},
                            CMetaBin::binary()).
handle_multi_upload_2({ok, Bin, Req}, _Req, Path,_ChunkedLen, BucketInfo, CMetaBin) ->
    %% trim spaces
    Acc = fun(#xmlText{value = " ",
                       pos = P}, Acc, S) ->
                  {Acc, P, S};
             (X, Acc, S) ->
                  {[X|Acc], S}
          end,
    {#xmlElement{content = Content},_} = xmerl_scan:string(
                                           binary_to_list(Bin),
                                           [{space,normalize}, {acc_fun, Acc}]),
    TotalUploadedObjs = length(Content),

    case handle_multi_upload_3(TotalUploadedObjs, Path, []) of
        {ok, {Len, ETag_1}} ->
            %% Retrieve the child object's metadata
            %% to set the actual chunked length
            IndexBin = list_to_binary(integer_to_list(1)),
            ChildKey = << Path/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,

            case leo_gateway_rpc_handler:head(ChildKey) of
                {ok, #?METADATA{del = 0,
                                dsize = ChildObjSize}} ->
                    case leo_gateway_rpc_handler:put(#put_req_params{path = Path,
                                                                     body = ?BIN_EMPTY,
                                                                     meta = CMetaBin,
                                                                     dsize = Len,
                                                                     msize = byte_size(CMetaBin),
                                                                     csize = ChildObjSize,
                                                                     total_chunks = TotalUploadedObjs,
                                                                     digest = ETag_1,
                                                                     bucket_info = BucketInfo}) of
                        {ok,_} ->
                            [BucketName|Path_1] = leo_misc:binary_tokens(Path, ?BIN_SLASH),
                            ETag2 = leo_hex:integer_to_hex(ETag_1, 32),
                            XML = gen_upload_completion_xml(
                                    BucketName, Path_1, ETag2, TotalUploadedObjs),
                            ?reply_ok([?SERVER_HEADER], XML, Req);
                        {error, unavailable} ->
                            ?reply_service_unavailable_error([?SERVER_HEADER], Path, <<>>, Req);
                        {error, Cause} ->
                            ?error("handle_multi_upload_2/5",
                                   [{key, binary_to_list(Path)}, {cause, Cause}]),
                            ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req)
                    end;
                _ ->
                    ?error("handle_multi_upload_2/5",
                           [{key, binary_to_list(Path)}, {cause, invalid_metadata}]),
                    ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req)
            end;
        {error, unavailable} ->
            ?reply_service_unavailable_error([?SERVER_HEADER], Path, <<>>, Req);
        {error, Cause} ->
            ?error("handle_multi_upload_2/5", [{key, binary_to_list(Path)}, {cause, Cause}]),
            ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req)
    end;
handle_multi_upload_2({error, Cause}, Req, Path,_ChunkedLen,_BucketInfo, _CMetaBin) ->
    ?error("handle_multi_upload_2/5", [{key, binary_to_list(Path)}, {cause, Cause}]),
    ?reply_internal_error([?SERVER_HEADER], Path, <<>>, Req).


%% @doc Retrieve Metadatas for uploaded objects (Multipart)
%% @private
-spec(handle_multi_upload_3(PartNum, Path, Acc) ->
             {ok, Ret} | {error, Cause} when PartNum::non_neg_integer(),
                                             Path::binary(),
                                             Acc::term(),
                                             Ret::{Len, ETag},
                                             Len::non_neg_integer(),
                                             ETag::binary(),
                                             Cause::any()).
handle_multi_upload_3(0,_Path, Acc) ->
    {Len, ETag} = lists:foldl(
                    fun({_, {DSize, Checksum}}, {Sum, ETagBin_1}) ->
                            ETagBin_2 = leo_hex:integer_to_raw_binary(Checksum),
                            {Sum + DSize, << ETagBin_1/binary, ETagBin_2/binary >>}
                    end, {0, <<>>}, lists:sort(
                                      lists:reverse(Acc))),
    ETag_1 = leo_hex:hex_to_integer(leo_hex:binary_to_hex(crypto:hash(md5, ETag))),
    {ok, {Len, ETag_1}};
handle_multi_upload_3(PartNum, Path, Acc) ->
    PartNumBin = list_to_binary(integer_to_list(PartNum)),
    Key = << Path/binary, ?STR_NEWLINE, PartNumBin/binary >>,

    case leo_gateway_rpc_handler:head(Key) of
        {ok, #?METADATA{dsize = Len,
                        checksum = Checksum}} ->
            handle_multi_upload_3(PartNum - 1, Path, [{PartNum, {Len, Checksum}} | Acc]);
        Error ->
            Error
    end.


%% @doc Generate an upload-key
%% @private
-spec(gen_upload_key(Path) ->
             Key when Path::binary(),
                      Key::string()).
gen_upload_key(Path) ->
    Key = lists:foldl(fun(I, []) ->
                              binary_to_list(I);
                         (I, Acc) ->
                              Acc ++ ?STR_SLASH ++ binary_to_list(I)
                      end, [], Path),
    Key.


%% @doc Generate an update-initiate xml
%% @private
-spec(gen_upload_initiate_xml(BucketNameBin, Path, UploadId) ->
             Ret when BucketNameBin::binary(),
                      Path::[binary()],
                      UploadId::binary(),
                      Ret::string()).
gen_upload_initiate_xml(BucketNameBin, Path, UploadId) ->
    BucketName = binary_to_list(BucketNameBin),
    Key = gen_upload_key(Path),
    io_lib:format(?XML_UPLOAD_INITIATION, [BucketName, Key, UploadId]).


%% @doc Generate an update-completion xml
%% @private
-spec(gen_upload_completion_xml(BucketNameBin, Path, ETag, Total) ->
             Ret when BucketNameBin::binary(),
                      Path::[binary()],
                      ETag::binary(),
                      Total::non_neg_integer(),
                      Ret::string()).
gen_upload_completion_xml(BucketNameBin, Path, ETag, Total) ->
    BucketName = binary_to_list(BucketNameBin),
    TotalStr = integer_to_list(Total),
    Key = gen_upload_key(Path),
    io_lib:format(?XML_UPLOAD_COMPLETION, [BucketName, Key, ETag, TotalStr]).


%% @doc Generate copy-obj's xml
%% @private
-spec(resp_copy_obj_xml(Req, Meta) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Meta::#?METADATA{}).
resp_copy_obj_xml(Req, Meta) ->
    XML = io_lib:format(?XML_COPY_OBJ_RESULT,
                        [leo_http:web_date(Meta#?METADATA.timestamp),
                         leo_hex:integer_to_hex(Meta#?METADATA.checksum, 32)]),
    ?reply_ok([?SERVER_HEADER,
               {?HTTP_HEAD_RESP_CONTENT_TYPE, ?HTTP_CTYPE_XML}
              ], XML, Req).


%% @doc Retrieve header values from a request
%%      Set request params
%% @private
-spec(request_params(Req, ReqParams) ->
             ReqParams when Req::cowboy_req:req(),
                            ReqParams::#req_params{}).
request_params(Req, Params) ->
    IsMultiDelete = case cowboy_req:qs_val(?HTTP_QS_BIN_MULTI_DELETE, Req) of
                        {undefined,_} ->
                            false;
                        _ ->
                            true
                    end,
    IsUpload = case cowboy_req:qs_val(?HTTP_QS_BIN_UPLOADS, Req) of
                   {undefined,_} ->
                       false;
                   _ ->
                       true
               end,
    UploadId = case cowboy_req:qs_val(?HTTP_QS_BIN_UPLOAD_ID, Req) of
                   {undefined,_} ->
                       <<>>;
                   {Val_1,_} ->
                       Val_1
               end,
    PartNum = case cowboy_req:qs_val(?HTTP_QS_BIN_PART_NUMBER, Req) of
                  {undefined,_} ->
                      0;
                  {Val_2,_} ->
                      list_to_integer(binary_to_list(Val_2))
              end,
    Range = element(1, cowboy_req:header(?HTTP_HEAD_RANGE, Req)),

    IsAwsChunked = case cowboy_req:header(?HTTP_HEAD_X_AMZ_CONTENT_SHA256, Req) of
                       {?HTTP_HEAD_X_VAL_AWS4_SHA256,_} ->
                           true;
                       _ ->
                           false
                   end,
    %% ?debug("request_params/2", "Is AWS Chunked: ~p", [IsAwsChunked]),

    {Headers, _} = cowboy_req:headers(Req),
    {ok, CMetaBin} = parse_headers_to_cmeta(Headers),

    case byte_size(CMetaBin) of
        MSize when MSize >= ?HTTP_METADATA_LIMIT ->
            {error, metadata_too_large};
        _ ->
            Params#req_params{is_multi_delete = IsMultiDelete,
                              is_upload = IsUpload,
                              is_aws_chunked = IsAwsChunked,
                              upload_id = UploadId,
                              upload_part_num = PartNum,
                              custom_metadata = CMetaBin,
                              range_header = Range}
    end.


%% @doc check if bucket is public-read
%% @private
-spec(is_public_read(BucketAclInfoL) ->
             Ret when BucketAclInfoL::[#bucket_acl_info{}],
                      Ret::boolean()).
is_public_read([]) ->
    false;
is_public_read([H|Rest]) ->
    #bucket_acl_info{user_id = UserId, permissions = Permissions} = H,
    case (UserId == ?GRANTEE_ALL_USER
          andalso (Permissions == [read] orelse Permissions == [read, write])) of
        true ->
            true;
        false ->
            is_public_read(Rest)
    end.


%% @private
-spec(is_public_read_write(BucketAclInfoL) ->
             Ret when BucketAclInfoL::[#bucket_acl_info{}],
                      Ret::boolean()).
is_public_read_write([]) ->
    false;
is_public_read_write([H|Rest]) ->
    #bucket_acl_info{user_id = UserId, permissions = Permissions} = H,
    case (UserId == ?GRANTEE_ALL_USER
          andalso (Permissions == [read, write])) of
        true ->
            true;
        false ->
            is_public_read_write(Rest)
    end.


%% @doc Authentication
%% @private
-spec(auth(Req, HTTPMethod, Path, TokenLen, ReqParams) ->
             {ok, AccessKeyId, {Signature, SignHead, SignKey}|undefined} |
             {error, Cause} when Req::cowboy_req:req(),
                                 HTTPMethod::binary(),
                                 Path::binary(),
                                 TokenLen::non_neg_integer(),
                                 ReqParams::#req_params{},
                                 AccessKeyId::binary(),
                                 Signature::binary(),
                                 SignHead::binary(),
                                 SignKey::binary(),
                                 Cause::any()).
auth(Req, HTTPMethod, Path, TokenLen, ReqParams) ->
    BucketName = case (TokenLen >= 1) of
                     true ->
                         erlang:hd(leo_misc:binary_tokens(Path, ?BIN_SLASH));
                     false ->
                         ?BIN_EMPTY
                 end,

    case leo_s3_bucket:get_latest_bucket(BucketName) of
        {ok, #?BUCKET{acls = ACLs} = Bucket} ->
            auth(Req, HTTPMethod, Path, TokenLen,
                 BucketName, ACLs, ReqParams#req_params{bucket_info = Bucket});
        not_found ->
            auth(Req, HTTPMethod, Path, TokenLen, BucketName, [], ReqParams);
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
-spec(auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams) ->
             {ok, AccessKeyId, {Signature, SignHead, SignKey}|undefined} |
             {error, Cause} when Req::cowboy_req:req(),
                                 HTTPMethod::binary(),
                                 Path::binary(),
                                 TokenLen::non_neg_integer(),
                                 BucketName::binary(),
                                 ACLs::[binary()],
                                 ReqParams::#req_params{},
                                 AccessKeyId::binary(),
                                 Signature::binary(),
                                 SignHead::binary(),
                                 SignKey::binary(),
                                 Cause::any()).
auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs,
     #req_params{is_multi_delete = true} = ReqParams) when TokenLen =< 1 ->
    case is_public_read_write(ACLs) of
        true ->
            {ok, <<>>, undefined};
        false ->
            auth_1(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams)
    end;
auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams) when TokenLen =< 1 ->
    auth_1(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams);
auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams) when TokenLen > 1,
                                                                        (HTTPMethod == ?HTTP_POST orelse
                                                                         HTTPMethod == ?HTTP_PUT  orelse
                                                                         HTTPMethod == ?HTTP_DELETE) ->
    case is_public_read_write(ACLs) of
        true ->
            {ok, <<>>, undefined};
        false ->
            auth_1(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams)
    end;
auth(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams) when TokenLen > 1 ->
    case is_public_read(ACLs) of
        true ->
            {ok, <<>>, undefined};
        false ->
            auth_1(Req, HTTPMethod, Path, TokenLen, BucketName, ACLs, ReqParams)
    end.

%% @private
auth_1(Req, HTTPMethod, Path, TokenLen, BucketName, _ACLs, #req_params{is_acl = IsACL}) ->
    case cowboy_req:header(?HTTP_HEAD_AUTHORIZATION, Req) of
        {undefined, _} ->
            {error, undefined};
        {AuthorizationBin, _} ->
            case AuthorizationBin of
                << Head:4/binary,
                   _Rest/binary >> when Head =:= ?HTTP_HEAD_X_AWS_SIGNATURE_V2;
                                        Head =:= ?HTTP_HEAD_X_AWS_SIGNATURE_V4 ->
                    IsCreateBucketOp = (TokenLen == 1 andalso
                                        HTTPMethod == ?HTTP_PUT andalso
                                        not IsACL),
                    {RawURI,_} = cowboy_req:path(Req),
                    {QStr,_} = cowboy_req:qs(Req),
                    {Headers,_} = cowboy_req:headers(Req),

                    %% NOTE:
                    %% - from s3cmd, dragondisk and others:
                    %%     -   Path: <<"photo/img">>
                    %%     - RawURI: <<"/img">>
                    %%
                    %% - from ruby-client, other AWS-clients:
                    %%     -   Path: <<"photo/img">>
                    %%     - RawURI: <<"/photo/img">>
                    %%
                    %% -> Adjust URI:
                    %%     #sign_params{ requested_uri = << "/photo/img" >>
                    %%                         raw_uri =  RawURI
                    %%                 }
                    %% * the hash-value is calculated by "raw_uri"
                    %%
                    Token_1 = leo_misc:binary_tokens(Path,   << ?STR_SLASH >>),
                    Token_2 = leo_misc:binary_tokens(RawURI, << ?STR_SLASH >>),
                    Path_1 = case (length(Token_1) /= length(Token_2)) of
                                 true ->
                                     << ?STR_SLASH, BucketName/binary, RawURI/binary >>;
                                 false ->
                                     case RawURI of
                                         << ?STR_SLASH, _/binary >> ->
                                             RawURI;
                                         _ ->
                                             << ?STR_SLASH, RawURI/binary >>
                                     end
                             end,

                    Len = byte_size(QStr),
                    QStr_2 = case (Len > 0 andalso binary:last(QStr) == $=) of
                                 true ->
                                     binary:part(QStr, 0, (Len - 1));
                                 false ->
                                     QStr
                             end,
                    QStr_3 = case binary:match(QStr_2, << "&" >>) of
                                 nomatch ->
                                     QStr_2;
                                 _ ->
                                     Ret = lists:foldl(
                                             fun(Q, []) ->
                                                     Q;
                                                (Q, Acc) ->
                                                     lists:append([Acc, "&", Q])
                                             end, [],
                                             lists:sort(string:tokens(binary_to_list(QStr_2), "&"))),
                                     list_to_binary(Ret)
                             end,
                    SignVer = case (Head =:= ?HTTP_HEAD_X_AWS_SIGNATURE_V4) of
                                  true ->
                                      v4;
                                  false ->
                                      v2
                              end,

                    SignParams = #sign_params{http_verb = HTTPMethod,
                                              content_md5 = ?http_header(Req, ?HTTP_HEAD_CONTENT_MD5),
                                              content_type = ?http_header(Req, ?HTTP_HEAD_CONTENT_TYPE),
                                              date = ?http_header(Req, ?HTTP_HEAD_DATE),
                                              bucket = BucketName,
                                              raw_uri = RawURI,
                                              requested_uri = Path_1,
                                              query_str = QStr_3,
                                              sign_ver = SignVer,
                                              headers = Headers,
                                              amz_headers = leo_http:get_amz_headers4cow(Headers)},
                    leo_s3_auth:authenticate(AuthorizationBin, SignParams, IsCreateBucketOp);
                _->
                    {error, nomatch}
            end
    end.


%% @doc Get bucket list
%% @private
%% @see http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketGET.html
-spec(get_bucket_1(AccessKeyId, Key, Delimiter, Marker, MaxKeys, Prefix) ->
             {ok, XMLRet} | {error, Cause} when AccessKeyId::binary(),
                                                Key::binary(),
                                                Delimiter::binary(),
                                                Marker::binary(),
                                                MaxKeys::non_neg_integer(),
                                                Prefix::binary()|none,
                                                XMLRet::binary(),
                                                Cause::any()).
get_bucket_1(AccessKeyId, <<>>, Delimiter, Marker, MaxKeys, none) ->
    get_bucket_1(AccessKeyId, ?BIN_SLASH, Delimiter, Marker, MaxKeys, none);
get_bucket_1(AccessKeyId, ?BIN_SLASH, _Delimiter, _Marker, _MaxKeys, none) ->
    case leo_s3_bucket:find_buckets_by_id(AccessKeyId) of
        not_found ->
            {ok, generate_bucket_xml([])};
        {ok, []} ->
            {ok, generate_bucket_xml([])};
        {ok, MetadataL} ->
            {ok, generate_bucket_xml(MetadataL)};
        Error ->
            Error
    end;
get_bucket_1(_AccessKeyId, BucketName, _Delimiter, _Marker, 0, Prefix) ->
    Prefix_1 = case Prefix of
                   none ->
                       <<>>;
                   _ ->
                       Prefix
               end,
    Path = << BucketName/binary, Prefix_1/binary >>,
    {ok, generate_bucket_xml(Path, Prefix_1, [], 0)};
get_bucket_1(_AccessKeyId, BucketName, none, Marker, MaxKeys, Prefix) ->
    ?debug("get_bucket_1/6", "BucketName: ~p, Marker: ~p, MaxKeys: ~p",
           [BucketName, Marker, MaxKeys]),
    Prefix_1 = case Prefix of
                   none ->
                       <<>>;
                   _ ->
                       Prefix
               end,

    {ok, #redundancies{nodes = Redundancies}} =
        leo_redundant_manager_api:get_redundancies_by_key(get, BucketName),
    Key = << BucketName/binary, Prefix_1/binary >>,

    case leo_gateway_rpc_handler:invoke(Redundancies,
                                        leo_storage_handler_directory,
                                        find_by_parent_dir,
                                        [Key, ?BIN_SLASH, Marker, MaxKeys],
                                        []) of
        {ok, Metadata} when is_list(Metadata) =:= true ->
            BodyFunc = fun(Socket, Transport) ->
                               BucketName_1 = erlang:hd(leo_misc:binary_tokens(BucketName, <<"/">>)),
                               HeadBin = generate_list_head_xml(BucketName_1, Prefix_1, MaxKeys, <<>>),
                               ok = Transport:send(Socket, HeadBin),
                               {ok, IsTruncated, NextMarker} =
                                   recursive_find(BucketName, Redundancies, Metadata,
                                                  Marker, MaxKeys, Transport, Socket),
                               FootBin = generate_list_foot_xml(IsTruncated, NextMarker),
                               ok = Transport:send(Socket, FootBin)
                       end,
            {ok, BodyFunc};
        {ok, _} ->
            {error, invalid_format};
        Error ->
            Error
    end;
get_bucket_1(_AccessKeyId, BucketName, Delimiter, Marker, MaxKeys, Prefix) ->
    ?debug("get_bucket_1/6", "BucketName: ~p, Delimiter: ~p, Marker: ~p, MaxKeys: ~p",
           [BucketName, Delimiter, Marker, MaxKeys]),

    Prefix_1 = case Prefix of
                   none ->
                       <<>>;
                   _ ->
                       Prefix
               end,

    {ok, #redundancies{nodes = Redundancies}} =
        leo_redundant_manager_api:get_redundancies_by_key(get, BucketName),
    Path = << BucketName/binary, Prefix_1/binary >>,

    case leo_gateway_rpc_handler:invoke(Redundancies,
                                        leo_storage_handler_directory,
                                        find_by_parent_dir,
                                        [Path, Delimiter, Marker, MaxKeys],
                                        []) of
        not_found ->
            {ok, generate_bucket_xml(Path, Prefix_1, [], MaxKeys)};
        {ok, []} ->
            {ok, generate_bucket_xml(Path, Prefix_1, [], MaxKeys)};
        {ok, MetadataL} ->
            {ok, generate_bucket_xml(Path, Prefix_1, MetadataL, MaxKeys)};
        Error ->
            Error
    end.


%% @doc Put a bucket
%% @private
%% @see http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketPUT.html
-spec(put_bucket_1(CannedACL, AccessKeyId, BucketName) ->
             ok | {error, Cause} when CannedACL::string(),
                                      AccessKeyId::binary(),
                                      BucketName::binary(),
                                      Cause::any()).
put_bucket_1([], AccessKeyId, BucketName) ->
    leo_s3_bucket:put(AccessKeyId, BucketName);
put_bucket_1(CannedACL, AccessKeyId, BucketName) ->
    leo_s3_bucket:put(AccessKeyId, BucketName, CannedACL).


%% @doc Put a bucket ACL
%% @private
%% @see http://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTacl.html
-spec(put_bucket_acl_1(CannedACL, AccessKeyId, BucketName) ->
             ok | {error, Cause} when CannedACL::string(),
                                      AccessKeyId::binary(),
                                      BucketName::binary(),
                                      Cause::any()).
put_bucket_acl_1(?CANNED_ACL_PRIVATE, AccessKeyId, BucketName) ->
    leo_s3_bucket:update_acls2private(AccessKeyId, BucketName);
put_bucket_acl_1(?CANNED_ACL_PUBLIC_READ, AccessKeyId, BucketName) ->
    leo_s3_bucket:update_acls2public_read(AccessKeyId, BucketName);
put_bucket_acl_1(?CANNED_ACL_PUBLIC_READ_WRITE, AccessKeyId, BucketName) ->
    leo_s3_bucket:update_acls2public_read_write(AccessKeyId, BucketName);
put_bucket_acl_1(_, _AccessKeyId, _BucketName) ->
    {error, not_supported}.


%% @doc Delete a bucket
%% @private
%% @see http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketDELETE.html
-spec(delete_bucket_1(AccessKeyId, BucketName) ->
             ok |  not_found | {error, Cause} when AccessKeyId::binary(),
                                                   BucketName::binary()|none,
                                                   Cause::any()).
delete_bucket_1(AccessKeyId, BucketName) ->
    BucketName_2 = formalize_bucket(BucketName),
    ManagerNodes = ?env_manager_nodes(leo_gateway),
    delete_bucket_2(ManagerNodes, AccessKeyId, BucketName_2).

%% @private
-spec(delete_bucket_2(NodeL, AccessKeyId, BucketName) ->
             ok |  not_found | {error, Cause} when NodeL::[atom()],
                                                   AccessKeyId::binary(),
                                                   BucketName::binary()|none,
                                                   Cause::any()).
delete_bucket_2([],_,_) ->
    {error, ?ERR_TYPE_INTERNAL_ERROR};
delete_bucket_2([Node|Rest], AccessKeyId, BucketName) ->
    Node_1 = case is_list(Node) of
                 true ->
                     list_to_atom(Node);
                 false ->
                     Node
             end,

    case rpc:call(Node_1, leo_manager_api, delete_bucket,
                  [AccessKeyId, BucketName], ?DEF_TIMEOUT) of
        ok ->
            ok;
        {error, not_found} ->
            not_found;
        {_, Cause} ->
            ?warn("delete_bucket_2/3", [{cause, Cause}]),
            delete_bucket_2(Rest, AccessKeyId, BucketName)
    end.


%% @doc Head a bucket
%% @private
%% @see http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketHEAD.html
-spec(head_bucket_1(AccessKeyId, BucketName) ->
             ok | not_found | {error, Cause} when AccessKeyId::binary(),
                                                  BucketName::binary(),
                                                  Cause::any()).
head_bucket_1(AccessKeyId, BucketName) ->
    leo_s3_bucket:head(AccessKeyId, BucketName).


%% @doc Generate XML from matadata-list
%% @private
-spec(generate_bucket_xml(PathBin, PrefixBin, MetadataL, MaxKeys) ->
             XMLRet when PathBin::binary(),
                         PrefixBin::binary(),
                         MetadataL::[#?METADATA{}],
                         MaxKeys::binary(),
                         XMLRet::string()).
generate_bucket_xml(PathBin, PrefixBin, MetadataL, MaxKeys) ->
    Bucket = erlang:hd(leo_misc:binary_tokens(PathBin, <<"/">>)),
    PathLen = byte_size(PathBin),
    Path = binary_to_list(PathBin),
    Prefix = binary_to_list(PrefixBin),

    Ref = make_ref(),
    ok = generate_bucket_xml_1(MetadataL, 1, Ref, PathLen, Path, Prefix, MaxKeys),

    TotalDivs = leo_math:ceiling(length(MetadataL) / ?DEF_MAX_NUM_OF_METADATAS),
    CallbackFun = fun(XMLList, NextMarker) ->
                          TruncatedStr = atom_to_list(length(MetadataL) =:= MaxKeys andalso MaxKeys =/= 0),
                          io_lib:format(?XML_OBJ_LIST,
                                        [xmerl_lib:export_text(Bucket),
                                         xmerl_lib:export_text(Prefix),
                                         integer_to_list(MaxKeys),
                                         XMLList,
                                         TruncatedStr,
                                         xmerl_lib:export_text(NextMarker)])
                  end,
    generate_bucket_xml_loop(Ref, TotalDivs, CallbackFun, []).

%% @private
-spec(generate_bucket_xml(MetadataL) ->
             XMLRet when MetadataL::[#?METADATA{}],
                         XMLRet::string()).
generate_bucket_xml(MetadataL) ->
    Fun = fun(#?BUCKET{name = BucketNameBin,
                       created_at = CreatedAt} , Acc) ->
                  BucketName = binary_to_list(BucketNameBin),
                  case string:equal(?STR_SLASH, BucketName) of
                      true ->
                          Acc;
                      false ->
                          lists:append([Acc,
                                        io_lib:format(?XML_BUCKET,
                                                      [xmerl_lib:export_text(BucketName),
                                                       leo_http:web_date(CreatedAt)])])
                  end
          end,
    io_lib:format(?XML_BUCKET_LIST, [lists:foldl(Fun, [], MetadataL)]).


%% @private
generate_bucket_xml_1([],_Index,_Ref,_PathLen,_Path,_Prefix,_MaxKeys) ->
    ok;
generate_bucket_xml_1(MetadataL, Index, Ref, PathLen, Path, Prefix, MaxKeys) ->
    {MetadataL_1, Rest} =
        case (length(MetadataL) >= ?DEF_MAX_NUM_OF_METADATAS) of
            true ->
                lists:split(?DEF_MAX_NUM_OF_METADATAS, MetadataL);
            false ->
                {MetadataL, []}
        end,

    PId = self(),
    spawn(fun() ->
                  Fun = fun(#?METADATA{key = EntryKeyBin,
                                       dsize = DSize,
                                       timestamp = Timestamp,
                                       checksum = Checksum,
                                       del = 0}, {Acc,_NextMarker}) ->
                                EntryKey = binary_to_list(EntryKeyBin),

                                case string:equal(Path, EntryKey) of
                                    true ->
                                        {Acc,_NextMarker};
                                    false ->
                                        Entry = string:sub_string(EntryKey, PathLen + 1),
                                        case (DSize == -1) of
                                            %% directory
                                            true ->
                                                {lists:append(
                                                   [Acc,
                                                    io_lib:format(?XML_DIR_PREFIX,
                                                                  [xmerl_lib:export_text(Prefix),
                                                                   xmerl_lib:export_text(Entry)])]),
                                                 EntryKeyBin};
                                            %% object
                                            false ->
                                                {lists:append(
                                                   [Acc,
                                                    io_lib:format(?XML_OBJ_LIST_FILE_2,
                                                                  [xmerl_lib:export_text(Prefix),
                                                                   xmerl_lib:export_text(Entry),
                                                                   leo_http:web_date(Timestamp),
                                                                   leo_hex:integer_to_hex(Checksum, 32),
                                                                   integer_to_list(DSize)])]),
                                                 EntryKeyBin}
                                        end
                                end
                        end,
                  {XMLList, NextMarker} = lists:foldl(Fun, {[], <<>>}, MetadataL_1),
                  erlang:send(PId, {append, Ref, {Index, XMLList, NextMarker}})
          end),
    generate_bucket_xml_1(Rest, Index + 1, Ref, PathLen, Path, Prefix, MaxKeys).


%% @private
generate_bucket_xml_loop(_Ref, 0, CallbackFun, Acc) ->
    {XMLList_1, NextMarker_1} =
        lists:foldl(fun({_Index, XMLList, NextMarker}, {SoFar,_}) ->
                            {lists:append([SoFar, XMLList]), NextMarker}
                    end, {[], []}, lists:sort(Acc)),
    CallbackFun(XMLList_1, NextMarker_1);
generate_bucket_xml_loop(Ref, TotalDivs, CallbackFun, Acc) ->
    receive
        {append, Ref, {Index, XMLList, NextMarker}} ->
            generate_bucket_xml_loop(Ref, TotalDivs - 1,
                                     CallbackFun, [{Index, XMLList, NextMarker}|Acc]);
        _ ->
            generate_bucket_xml_loop(Ref, TotalDivs, CallbackFun, Acc)
    after
        ?DEF_REQ_TIMEOUT ->
            {error, timeout}
    end.



%% @doc Generate XML from ACL
%% @private
-spec(generate_acl_xml(BucketInfo) ->
             XMLRet when BucketInfo::#?BUCKET{},
                         XMLRet::string()).
generate_acl_xml(#?BUCKET{access_key_id = ID, acls = ACLs}) ->
    Fun = fun(#bucket_acl_info{user_id = URI,
                               permissions = Permissions} , Acc) ->
                  lists:foldl(
                    fun(read, Acc_1) ->
                            lists:flatten(
                              lists:append(
                                [Acc_1,
                                 io_lib:format(?XML_ACL_GRANT, [URI, ?acl_read]),
                                 io_lib:format(?XML_ACL_GRANT, [URI, ?acl_read_acp])
                                ]));
                       (write, Acc_1) ->
                            lists:flatten(
                              lists:append(
                                [Acc_1,
                                 io_lib:format(?XML_ACL_GRANT, [URI, ?acl_write]),
                                 io_lib:format(?XML_ACL_GRANT, [URI, ?acl_write_acp])
                                ]));
                       (full_control, Acc_1) ->
                            lists:append(
                              [Acc_1,
                               io_lib:format(?XML_ACL_GRANT, [URI, ?acl_full_control])])
                    end, Acc, Permissions)
          end,
    io_lib:format(?XML_ACL_POLICY, [ID, ID, lists:foldl(Fun, [], ACLs)]).

%% @private
-spec(generate_delete_multi_xml(IsQuiet, DeletedKeys, ErrorKeys) ->
             XMLRet when IsQuiet::boolean(),
                         DeletedKeys::[binary()],
                         ErrorKeys::[binary()],
                         XMLRet::string()).
generate_delete_multi_xml(IsQuiet, DeletedKeys, ErrorKeys) ->
    DeletedElems = generate_delete_multi_xml_deleted_elem(DeletedKeys, []),
    ErrorElems = case IsQuiet of
                     true ->
                         [];
                     false ->
                         generate_delete_multi_xml_error_elem(ErrorKeys, [])
                 end,
    io_lib:format(?XML_MULTIPLE_DELETE, [DeletedElems, ErrorElems]).

%% @private
generate_delete_multi_xml_deleted_elem([], Acc) ->
    Acc;
generate_delete_multi_xml_deleted_elem([DeletedKey|Rest], Acc) ->
    generate_delete_multi_xml_deleted_elem(
      Rest, lists:append([Acc,
                          io_lib:format(?XML_MULTIPLE_DELETE_SUCCESS_ELEM, [DeletedKey])])).

%% @private
generate_delete_multi_xml_error_elem([], Acc) ->
    Acc;
generate_delete_multi_xml_error_elem([ErrorKey|Rest], Acc) ->
    generate_delete_multi_xml_deleted_elem(
      Rest, lists:append([Acc,
                          io_lib:format(?XML_MULTIPLE_DELETE_ERROR_ELEM, [ErrorKey])])).


%% @doc Delete multiple objects, then parse request XML
%% @private
-spec(delete_multi_objects_2(Req, Body, MD5, MD5, Params) ->
             {ok, Req} when Req::cowboy_req:req(),
                            Body::binary(),
                            MD5::binary(),
                            Params::#req_params{}).
delete_multi_objects_2(Req, Body, MD5, MD5, Params) ->
    Acc = fun(#xmlText{value = " ", pos = P}, Acc, S) ->
                  {Acc, P, S};
             (X, Acc, S) ->
                  {[X|Acc], S}
          end,
    try
        {#xmlElement{content = Content},_} =
            xmerl_scan:string(binary_to_list(Body),
                              [{space,normalize}, {acc_fun, Acc}]),
        delete_multi_objects_3(Req, Content, false, [], Params)
    catch _:Cause ->
            ?error("delete_multi_objects_2/5", [{req, Req}, {cause, Cause}]),
            ?reply_malformed_xml([?SERVER_HEADER], Req)
    end;
delete_multi_objects_2(Req, _Body, _MD5, _, _Params) ->
    ?reply_bad_digest([?SERVER_HEADER], <<>>, <<>>, Req).


%% @doc Retrieve every keys (ignore version element)
%% @private
delete_multi_objects_3(Req, [], IsQuiet, Keys, Params) ->
    delete_multi_objects_4(Req, IsQuiet, Keys, [], [], Params);
delete_multi_objects_3(Req, [#xmlElement{name = 'Quiet'}|Rest], _IsQuiet, Keys, Params) ->
    delete_multi_objects_3(Req, Rest, true, Keys, Params);
delete_multi_objects_3(Req, [#xmlElement{name = 'Object', content = KeyElem}|Rest], IsQuiet, Keys, Params) ->
    [#xmlElement{content = TextElem}|_] = KeyElem,
    [#xmlText{value = Key}|_] = TextElem,
    delete_multi_objects_3(Req, Rest, IsQuiet, [Key|Keys], Params);
delete_multi_objects_3(Req, [_|Rest], IsQuiet, Keys, Params) ->
    delete_multi_objects_3(Req, Rest, IsQuiet, Keys, Params).


%% @doc Issue delete requests for all keys by using leo_gateway_rpc_handler:delete
%% @private
delete_multi_objects_4(Req, IsQuiet, [], DeletedKeys, ErrorKeys, Params) ->
    delete_multi_objects_5(Req, IsQuiet, DeletedKeys, ErrorKeys, Params);
delete_multi_objects_4(Req, IsQuiet, [Key|Rest], DeletedKeys, ErrorKeys,
                       #req_params{bucket_name = BucketName} = Params) ->
    BinKey = list_to_binary(Key),
    Path = << BucketName/binary, <<"/">>/binary, BinKey/binary >>,
    case leo_gateway_rpc_handler:head(Path) of
        {ok, Meta} ->
            BeginTime = leo_date:clock(),
            case leo_gateway_rpc_handler:delete(Path) of
                ok ->
                    ?access_log_delete(BucketName, Path, Meta#?METADATA.dsize, ?HTTP_ST_NO_CONTENT, BeginTime),
                    delete_multi_objects_4(Req, IsQuiet, Rest,
                                           [Key|DeletedKeys], ErrorKeys, Params);
                {error, not_found} ->
                    delete_multi_objects_4(Req, IsQuiet, Rest,
                                           [Key|DeletedKeys], ErrorKeys, Params);
                {error, _} ->
                    delete_multi_objects_4(Req, IsQuiet, Rest,
                                           DeletedKeys, [Key|ErrorKeys], Params)
            end;
        _ ->
            delete_multi_objects_4(Req, IsQuiet, Rest,
                                   DeletedKeys, [Key|ErrorKeys], Params)
    end.

%% @doc Make response XML based on the result of delete requests (ignore version related elements)
%% @private
delete_multi_objects_5(Req, IsQuiet, DeletedKeys, ErrorKeys, _Params) ->
    XML = generate_delete_multi_xml(IsQuiet, DeletedKeys, ErrorKeys),
    %% 6. Respond the response XML
    ?reply_ok([?SERVER_HEADER,
               {?HTTP_HEAD_RESP_CONTENT_TYPE, ?HTTP_CTYPE_XML}
              ], XML, Req).


%% @private
-spec(formalize_bucket(BucketName) ->
             BucketName when BucketName::binary()).
formalize_bucket(BucketName) ->
    case (binary:last(BucketName) == $/) of
        true ->
            binary:part(BucketName, {0, byte_size(BucketName) - 1});
        false ->
            BucketName
    end.

generate_list_head_xml(BucketName, Prefix, MaxKeys, Delimiter) ->
    Delimiter_1 = case Delimiter of
                      <<>> ->
                          ?DEF_DELIMITER;
                      _ ->
                          Delimiter
                  end,
    io_lib:format(?XML_OBJ_LIST_HEAD,
                  [xmerl_lib:export_text(BucketName),
                   xmerl_lib:export_text(Prefix),
                   integer_to_list(MaxKeys),
                   xmerl_lib:export_text(Delimiter_1)]).

generate_list_foot_xml(IsTruncated, NextMarker) ->
    TruncatedStr = case IsTruncated of
                       true ->
                           << "true" >>;
                       false ->
                           << "false" >>
                   end,
    io_lib:format(?XML_OBJ_LIST_FOOT,
                  [TruncatedStr,
                   xmerl_lib:export_text(NextMarker)]).

generate_list_file_xml(BucketName, #?METADATA{key = Key,
                                              dsize = Length,
                                              timestamp = TS,
                                              checksum = CS,
                                              del = 0}) ->
    BucketNameLen = byte_size(BucketName),
    << _:BucketNameLen/binary, Key_1/binary >> = Key,
    io_lib:format(?XML_OBJ_LIST_FILE_1,
                  [xmerl_lib:export_text(Key_1),
                   leo_http:web_date(TS),
                   leo_hex:integer_to_hex(CS, 32),
                   integer_to_list(Length)]);
generate_list_file_xml(_,_) ->
    error.


%% @doc Recursively find a key in the bucket
%% @private
-spec(recursive_find(BucketName, Redundancies, MetadataList,
                     Marker, MaxKeys, Transport, Socket) ->
             {ok, CanFindKey, LastKey} | {error, any()} when BucketName::binary(),
                                                             Redundancies::[#redundancies{}],
                                                             MetadataList::[#?METADATA{}],
                                                             Marker::binary(),
                                                             MaxKeys::non_neg_integer(),
                                                             Transport::atom(),
                                                             Socket::port(),
                                                             CanFindKey::boolean(),
                                                             LastKey::binary()).
recursive_find(BucketName, Redundancies, MetadataList,
               Marker, MaxKeys, Transport, Socket) ->
    recursive_find(BucketName, Redundancies, [], MetadataList,
                   Marker, MaxKeys, <<>>, Transport, Socket).

recursive_find(_BucketName, _Redundancies,_,_,_, 0, LastKey,_,_) ->
    {ok, true, LastKey};
recursive_find(_BucketName, _Redundancies,[],[],_,_,_,_,_) ->
    {ok, false, <<>>};
recursive_find(BucketName, Redundancies, [Head|Rest], [],
               Marker, MaxKeys, LastKey, Transport, Socket) ->
    recursive_find(BucketName, Redundancies, Rest, Head,
                   Marker, MaxKeys, LastKey, Transport, Socket);
recursive_find(BucketName, Redundancies, Acc,
               [#?METADATA{dsize = -1, key = Key}|Rest],
               Marker, MaxKeys, LastKey, Transport, Socket) ->
    case leo_gateway_rpc_handler:invoke(Redundancies,
                                        leo_storage_handler_directory,
                                        find_by_parent_dir,
                                        [Key, ?BIN_SLASH, Marker, MaxKeys],
                                        []) of
        {ok, Metadata} when is_list(Metadata) ->
            recursive_find(BucketName, Redundancies, [Rest | Acc], Metadata,
                           Marker, MaxKeys, LastKey, Transport, Socket);
        {ok,_} ->
            {error, invalid_format};
        Error ->
            Error
    end;
recursive_find(BucketName, Redundancies, Acc,
               [#?METADATA{key = Key} = Head|Rest],
               Marker, MaxKeys, LastKey, Transport, Socket) ->
    case generate_list_file_xml(BucketName, Head) of
        error ->
            recursive_find(BucketName, Redundancies, Acc, Rest,
                           MaxKeys, MaxKeys, LastKey, Transport, Socket);
        Bin ->
            case Transport:send(Socket, Bin) of
                ok ->
                    recursive_find(BucketName, Redundancies, Acc, Rest,
                                   Marker, MaxKeys - 1, Key, Transport, Socket);
                Error ->
                    Error
            end
    end.

%% @doc parse Custom Meta from Headers
-spec(parse_headers_to_cmeta(Headers) ->
    {ok, Bin} | {error, Cause} when Headers::list(),
                                    Bin::binary(),
                                    Cause::any()).
parse_headers_to_cmeta(Headers) when is_list(Headers) ->
    MetaList = lists:foldl(fun(Ele, Acc) ->
                                   case Ele of
                                       {<<"x-amz-meta-", _/binary>>, _} ->
                                           [Ele | Acc];
                                       _ ->
                                           Acc
                                   end
                           end, [], Headers),
    case MetaList of
        [] ->
            {ok, <<>>};
        _ ->
            {ok, term_to_binary(MetaList)}
    end;
parse_headers_to_cmeta(_) ->
    {error, badarg}.
