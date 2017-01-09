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
%% Leo Gateway - HTTP-behabiour
%% @doc
%% @end
%%======================================================================
-module(leo_gateway_http_behaviour).

-include("leo_http.hrl").

%% Bucket handlers
-callback(get_bucket(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(put_bucket(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(delete_bucket(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(head_bucket(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).

%% Object handlers
-callback(get_object(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(put_object(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(delete_object(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(head_object(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(range_object(Req::cowboy_req:req(), Key::binary(), HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
-callback(get_object_with_cache(Req::cowboy_req:req(), Key::binary(),
                                CacheObj::#cache{}, HttpParams::#req_params{}) ->
                 {ok, Req::cowboy_req:req()}).
