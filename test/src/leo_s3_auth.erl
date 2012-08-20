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
         get_signature/2
        ]).

%% @doc Generate a signature.
%% @private
-define(SUB_RESOURCES, ["?acl", "?location", "?logging", "?torrent"]).

-spec(get_signature(string(), #sign_params{}) ->
             string()).
get_signature(SecretAccessKey, SignParams) ->
    #sign_params{http_verb    = HTTPVerb,
                 content_md5  = ETag,
                 content_type = ContentType,
                 date         = Date0,
                 bucket       = Bucket0,
                 uri          = URI0,
                 query_str    = QueryStr,
                 amz_headers  = AmzHeaders
                } = SignParams,

    Date1   = auth_date(Date0, AmzHeaders),
    Sub0    = auth_resources(AmzHeaders),
    Sub1    = auth_sub_resources(QueryStr),
    Bucket1 = auth_bucket(URI0, Bucket0, QueryStr),
    URI1    = auth_uri(Bucket1, URI0),
    %% ?debugVal({Date1, Sub0, Sub1, Bucket1, URI1}),

    StringToSign = lists:flatten(
                     io_lib:format("~s\n~s\n~s\n~s~s~s~s~s",
                                   [HTTPVerb, ETag, ContentType,
                                    Date1, Sub0, Bucket1, URI1, Sub1])),
    Signature = binary_to_list(
                  base64:encode(
                    crypto:sha_mac(SecretAccessKey, StringToSign))),

    %% ?debugVal({StringToSign, Signature}),
    Signature.


%%--------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Retrieve date
%% @private
auth_date(Date0, CannonocalizedResources) ->
    case lists:keysearch("x-amz-date", 1, CannonocalizedResources) of
        {value, _} -> [];
        false      -> Date0 ++ "\n"
    end.


%% @doc Retrieve a bucket from string
%% @private
auth_bucket("/",_Bucket, []) -> [];
auth_bucket("/", Bucket,  _) -> "/" ++ Bucket;
auth_bucket(_,   [],      _) -> [];
auth_bucket(_,   Bucket,  _) -> "/" ++ Bucket.


%% @doc Retrieve URI
%% @private
auth_uri(Bucket, URI) ->
    case (string:str(URI, Bucket) == 1) of
        true  -> string:sub_string(URI, (length(Bucket) + 1));
        false -> URI
    end.


%% @doc Retrieve resources
%% @private
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
            [];
        Headers ->
            lists:foldl(fun({K2, V2}, Acc1) ->
                                Acc1 ++ K2 ++ ":" ++ V2 ++ "\n"
                        end, [], Headers)
    end.


%% @doc Retrieve sub-resources
%% @private
auth_sub_resources(QueryStr) ->
    lists:foldl(fun(Param, [] = Acc) ->
                        case (string:str(QueryStr, Param) > 0) of
                            true  -> Param;
                            false -> Acc
                        end;
                   (_, Acc) ->
                        Acc
                end, [], ?SUB_RESOURCES).

