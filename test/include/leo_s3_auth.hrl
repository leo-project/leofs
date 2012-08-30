%%======================================================================
%%
%% Leo Auth
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
%% Leo Auth
%% @doc
%% @end
%%======================================================================
%% @doc Credential data
%%
-record(credential, {
          access_key_id     :: string(),
          secret_access_key :: string(),
          user_id           :: string(),
          created_at        :: integer()
         }).

%% @doc Authentication Info
%%
-record(auth_info, {
          db            :: atom(), %% db-type:[ets | mnesia]
          provider = [] :: list()  %% auth-info provides
         }).

%% @doc AMZ-S3-API related
%%
-record(sign_params, {
          http_verb    = "" :: string(),
          content_md5  = "" :: string(),
          content_type = "" :: string(),
          date         = "" :: string(),
          bucket       = "" :: string(),
          uri          = "" :: string(),
          query_str    = "" :: string(),
          sub_resource = "" :: string(), %% [?acl" | "?location" | "?logging" | "?torrent"]
          amz_headers  = [] :: list()
         }).

