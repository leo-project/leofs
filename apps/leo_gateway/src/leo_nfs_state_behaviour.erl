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
%%======================================================================
-module(leo_nfs_state_behaviour).

-callback(init(Params::any()) ->
                 ok | {error, any()}).

-callback(add_path(Path4S3::binary()) ->
                 {ok, binary()}| {error, any()}).

-callback(get_path(UID::binary()) ->
                 {ok, binary()}| not_found).

-callback(del_path(UID::binary()) ->
                 true).

-callback(get_uid_list() ->
                 {ok, list(binary())}| {error, any()}).

-callback(get_path_list() ->
                 {ok, list(binary())}| {error, any()}).

-callback(add_readdir_entry(CookieVerf::binary(), ReadDirEntry::list()) ->
                 true).

-callback(get_readdir_entry(CookieVerf::binary()) ->
                 {ok, list()}| not_found).

-callback(del_readdir_entry(CookieVerf::binary()) ->
                 true).

-callback(add_write_verfier(WriteVerf::binary()) ->
                 true).

-callback(get_write_verfier() ->
                 {ok, binary()}| not_found).
