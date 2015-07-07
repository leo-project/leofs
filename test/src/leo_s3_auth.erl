%%======================================================================
%%
%% Leo S3-Libs
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
%% Leo Libs - Auth
%% @doc
%% @end
%%======================================================================
-module(leo_s3_auth).

-author('Yosuke Hara').

-include("leo_s3_auth.hrl").

-export([
         get_signature/3
        ]).

%% @doc Generate a signature.
%% @private
-define(SUB_RESOURCES, [<<"acl">>,
                        <<"delete">>,
                        <<"lifecycle">>,
                        <<"location">>,
                        <<"logging">>,
                        <<"notification">>,
                        <<"partNumber">>,
                        <<"policy">>,
                        <<"requestPayment">>,
                        <<"torrent">>,
                        <<"uploadId">>,
                        <<"uploads">>,
                        <<"versionid">>,
                        <<"versioning">>,
                        <<"versions">>,
                        <<"website">>,
                        <<"response-content-type">>,
                        <<"response-content-language">>,
                        <<"response-expires">>,
                        <<"response-cache-control">>,
                        <<"response-content-disposition">>,
                        <<"response-content-encoding">>]).

-spec(get_signature(SecretAccessKey, SignParams, SignV4Params) ->
             binary() when SecretAccessKey::binary(),
                           SignParams::#sign_params{},
                           SignV4Params::#sign_v4_params{}).
get_signature(SecretAccessKey, #sign_params{sign_ver = Ver} = SignParams, SignV4Params) ->
    get_signature_1(Ver, SecretAccessKey, SignParams, SignV4Params).

%% @doc Get AWS signature for v4/v2
%% @private
-spec(get_signature_1(AuthVer, SecretAccessKey, SignParams, SignV4Params) ->
             {SignatureBin, BinToSignHead, SigningKey}
                 when AuthVer::aws_sign_ver(),
                      SecretAccessKey::binary(),
                      SignParams::#sign_params{},
                      SignV4Params::#sign_v4_params{}|undefined,
                      SignatureBin::binary(),
                      BinToSignHead::binary(),
                      SigningKey::binary()).
get_signature_1(?AWS_SIGN_VER_4, SecretAccessKey, SignParams, SignV4Params) ->
    #sign_params{http_verb      = HTTPVerb,
                 date           = Date,
                 raw_uri        = URI,
                 query_str      = QueryStr,
                 headers        = Headers
                } = SignParams,
    #sign_v4_params{credential      = Credential,
                    signed_headers  = SignedHeaders
                   } = SignV4Params,
    Header_1    = auth_v4_headers(Headers, SignedHeaders),
    Hash_2      = case lists:keyfind(<<"x-amz-content-sha256">>, 1, Headers) of
                      false ->
                          <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>;
                      {_, Hash_1} ->
                          Hash_1
                  end,
    QueryStr_1  = auth_v4_qs(QueryStr),
    Request_1   = <<HTTPVerb/binary,        "\n",
                    URI/binary,             "\n",
                    QueryStr_1/binary,      "\n",
                    Header_1/binary,        "\n",
                    SignedHeaders/binary,   "\n",
                    Hash_2/binary>>,

    RequestHash = crypto:hash(sha256, Request_1),
    Date_1      = auth_v4_date(Date, Headers),
    [_AWSAccessKeyId, Date_2, Region, Service, <<"aws4_request">>] = binary:split(Credential, <<"/">>, [global]),
    Scope = <<Date_2/binary, "/", Region/binary, "/", Service/binary, "/aws4_request">>,
    RequestBin = leo_hex:binary_to_hexbin(RequestHash),
    BinToSignHead   = <<Date_1/binary, "\n",
                        Scope/binary,  "\n">>,
    BinToSign       = <<"AWS4-HMAC-SHA256\n",
                        BinToSignHead/binary,
                        RequestBin/binary>>,

    DateKey       = crypto:hmac(sha256, <<"AWS4", SecretAccessKey/binary>>, Date_2),
    DateRegionKey = crypto:hmac(sha256, DateKey, Region),
    DateRegionServiceKey = crypto:hmac(sha256, DateRegionKey, Service),
    SigningKey   = crypto:hmac(sha256, DateRegionServiceKey, <<"aws4_request">>),
    Signature    = crypto:hmac(sha256, SigningKey, BinToSign),
    SignatureBin = leo_hex:binary_to_hexbin(Signature),
    {SignatureBin, BinToSignHead, SigningKey};

get_signature_1(?AWS_SIGN_VER_2, SecretAccessKey, SignParams, _) ->
    #sign_params{http_verb     = HTTPVerb,
                 content_md5   = ETag,
                 content_type  = ContentType,
                 date          = Date,
                 bucket        = Bucket,
                 raw_uri       = URI,
                 requested_uri = RequestedURI,
                 query_str     = QueryStr,
                 amz_headers   = AmzHeaders
                } = SignParams,
    Date_1  = auth_date(Date, AmzHeaders),
    Sub_1   = auth_resources(AmzHeaders),
    Sub_2   = auth_sub_resources(QueryStr),
    Bucket1 = auth_bucket(URI, Bucket, QueryStr),
    URI_1   = auth_uri(Bucket, URI, RequestedURI),
    BinToSign = <<HTTPVerb/binary,    "\n",
                  ETag/binary,        "\n",
                  ContentType/binary, "\n",
                  Date_1/binary,       "\n",
                  Sub_1/binary, Bucket1/binary, URI_1/binary, Sub_2/binary>>,
    Context = crypto:hmac_init(sha, SecretAccessKey),
    Context_1 = crypto:hmac_update(Context, BinToSign),
    HMac = crypto:hmac_final(Context_1),
    Signature = base64:encode(HMac),
    {Signature, <<>>, <<>>}.

%%--------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Construct Canonical Headers
%% @private
auth_v4_headers(Headers, SignedHeaders) ->
    HeaderList = binary:split(SignedHeaders, <<";">>, [global]),
    auth_v4_headers(Headers, HeaderList, <<>>).

%% @private
auth_v4_headers(_Headers, [], Acc) ->
    Acc;
auth_v4_headers(Headers, [Head|Rest], Acc) ->
    Val = case lists:keyfind(Head, 1, Headers) of
              false ->
                  <<>>;
              {_, Bin} ->
                  leo_hex:binary_trim(Bin)
          end,
    auth_v4_headers(Headers, Rest, <<Acc/binary, Head/binary, ":", Val/binary, "\n">>).

%% @doc Consutrct Canonical Query String
%% @private
auth_v4_qs(QueryStr) ->
    List = cow_qs:parse_qs(QueryStr),
    lists:foldl(fun({Key, Val}, Acc) ->
                        KeyBin = cow_qs:urlencode(Key),
                        ValBin = case Val of
                                     true ->
                                         <<>>;
                                     _ ->
                                         cow_qs:urlencode(Val)
                                 end,
                        case Acc of
                            <<>> ->
                                <<KeyBin/binary, "=", ValBin/binary>>;
                            _ ->
                                <<Acc/binary, "&", KeyBin/binary, "=", ValBin/binary>>
                        end
                end, <<>>, List).

%% @doc Retrieve date V4
%% @private
-spec(auth_v4_date(Date, Headers) ->
             binary() when Date::binary(),
                           Headers::list()).
auth_v4_date(Date, Headers) ->
    case lists:keyfind(<<"x-amz-date">>, 1, Headers) of
        false ->
            Date;
        {<<"x-amz-date">>, Date_2} ->
            Date_2
    end.

%% @doc Retrieve date
%% @private
-spec(auth_date(Date, CannonocalizedResources) ->
             binary() when Date::binary(),
                           CannonocalizedResources::list()).
auth_date(Date, CannonocalizedResources) ->
    case lists:keysearch("x-amz-date", 1, CannonocalizedResources) of
        {value, _} ->
            <<>>;
        false ->
            << Date/binary >>
    end.


%% @doc Retrieve a bucket from string
%% @private
%% auth_bucket("/",_Bucket, []) -> [];
%% auth_bucket(<<"/">>, Bucket,  _) -> << <<"/">>, Bucket >>;
-spec(auth_bucket(URI, Bucket, QueryStr) ->
             binary() when URI::binary(),
                           Bucket::binary(),
                           QueryStr::binary()).
auth_bucket(_, <<>>,  _) -> <<>>;
auth_bucket(_, Bucket,_) -> << <<"/">>/binary, Bucket/binary >>.

-spec(auth_uri(Bucket, URI, RequestedURI) ->
             binary() when Bucket::binary(),
                           URI::binary(),
                           RequestedURI::binary()).
auth_uri(<<>>, URI,_URI) ->
    URI;
auth_uri(_Bucket,<<"/">> = URI,_URI) ->
    URI;
auth_uri(Bucket,_URI, URI) ->
    case binary:match(URI, Bucket) of
        {1, _} ->
            BucketLen = byte_size(Bucket),
            BucketThresholdLen1 = BucketLen + 1,
            BucketThresholdLen2 = BucketLen + 2,
            URILen = byte_size(URI),

            case URILen of
                BucketThresholdLen1 ->
                    remove_duplicated_bucket(Bucket, URI);
                BucketThresholdLen2 ->
                    <<"/", Bucket:BucketLen/binary, LastChar:8>> = URI,
                    case LastChar == $/ of
                        true ->
                            %% /${Bucket}/ pattern are should be removed
                            remove_duplicated_bucket(Bucket, URI);
                        false ->
                            %% ex. /${Bucket}.
                            URI
                    end;
                _ ->
                    SegmentLen = length(binary:split(URI, <<"/">>, [global])),
                    case (SegmentLen >= 3) of
                        true ->
                            %% ex. /${Bucket}/path_to_file
                            remove_duplicated_bucket(Bucket, URI);
                        false ->
                            %% /${Bucket}[^/]+ pattern are should not be removed
                            URI
                    end
            end;
        _ ->
            URI
    end.


%% @doc remove duplicated bucket's name from path
%% @private
-spec(remove_duplicated_bucket(Bucket, URI) ->
             binary() when Bucket::binary(),
                           URI::binary()).
remove_duplicated_bucket(Bucket, URI) ->
    SkipSize = size(Bucket) + 1,
    binary:part(URI, {SkipSize, size(URI) - SkipSize}).


%% @doc Retrieve resources
%% @private
-spec(auth_resources(CannonocalizedResources) ->
             binary() when CannonocalizedResources::list()).
auth_resources(CannonocalizedResources) ->
    case lists:foldl(fun({K0, V0}, Acc0) ->
                             K1 = string:to_lower(K0),
                             case lists:keysearch(K1, 1, Acc0) of
                                 {value, {_, V1}} ->
                                     ordsets:add_element({K1, V1 ++ "," ++ V0},
                                                         lists:keydelete(K1, 1, Acc0));
                                 false ->
                                     ordsets:add_element({K1, V0}, Acc0)
                             end
                     end, [], CannonocalizedResources) of
        [] ->
            <<>>;
        Headers ->
            lists:foldl(fun({K2, V2}, Acc1) ->
                                BinKey =  list_to_binary(K2),
                                BinVal =  list_to_binary(V2),
                                <<Acc1/binary, BinKey/binary, ":", BinVal/binary, "\n" >>
                        end, <<>>, Headers)
    end.


%% @doc Retrieve sub-resources
%% @private
%% QueryStr must be sorted lexicographically by param name at caller
-spec(auth_sub_resources(QueryStr) ->
             binary() when QueryStr::binary()).
auth_sub_resources(QueryStr) ->
    ParamList = binary:split(QueryStr, [<<"?">>, <<"&">>], [global]),
    lists:foldl(fun(<<>>, Acc) ->
                        %% ignore empty elements
                        Acc;
                   (Param, <<>>) ->
                        %% append '?' to first param
                        [Key|Rest] = binary:split(Param, <<"=">>),
                        case binary:match(Key, ?SUB_RESOURCES) of
                            nomatch -> <<>>;
                            _ ->
                                case Rest of
                                    [] -> <<"?", Key/binary>>;
                                    [Val|_] ->
                                        DecodedVal = cow_qs:urldecode(Val),
                                        <<"?", Key/binary, "=", DecodedVal/binary>>
                                end
                        end;
                   (Param, Acc) ->
                        %% append '&' to other params
                        [Key|Rest] = binary:split(Param, <<"=">>),
                        case binary:match(Key, ?SUB_RESOURCES) of
                            nomatch -> Acc;
                            _ ->
                                case Rest of
                                    [] -> <<Acc/binary, "&", Key/binary>>;
                                    [Val|_] ->
                                        DecodedVal = cow_qs:urldecode(Val),
                                        <<Acc/binary, "&", Key/binary, "=", DecodedVal/binary>>
                                end
                        end
                end, <<>>, ParamList).
