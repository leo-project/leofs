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
-module(leo_nfs_state_ets).

-behaviour(leo_nfs_state_behaviour).

-export([init/1,
         add_path/1, get_path/1, del_path/1, get_uid_list/0, get_path_list/0,
         add_readdir_entry/2, get_readdir_entry/1, del_readdir_entry/1,
         add_write_verfier/1, get_write_verfier/0]).


-define(LEO_GW_NFS_UID2PATH_ETS_TBL, uid2path_ets_tbl).
-define(LEO_GW_NFS_PATH2UID_ETS_TBL, path2uid_ets_tbl).
-define(LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, readdir_entry_ets_tbl).
-define(LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL, write_verifier_ets_tbl).


%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------
%% @doc
init(_Params) ->
    ets:new(?LEO_GW_NFS_UID2PATH_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_PATH2UID_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL, [set, named_table, public]),
    ok.


%% @doc
add_path(Path4S3) ->
    Ret = ets:lookup(?LEO_GW_NFS_PATH2UID_ETS_TBL, Path4S3),
    case Ret of
        [] ->
            NewUID = v4(),
            ets:insert(?LEO_GW_NFS_UID2PATH_ETS_TBL, {NewUID, Path4S3}),
            ets:insert(?LEO_GW_NFS_PATH2UID_ETS_TBL, {Path4S3, NewUID}),
            {ok, NewUID};
        [{_, UID}|_] ->
            %% already exists
            {ok, UID}
    end.


%% @doc
get_path(UID) ->
    Ret = ets:lookup(?LEO_GW_NFS_UID2PATH_ETS_TBL, UID),
    case Ret of
        [] ->
            not_found;
        [{_, Path4S3}|_] ->
            {ok, Path4S3}
    end.


%% @doc
del_path(UID) ->
    Ret = ets:lookup(?LEO_GW_NFS_UID2PATH_ETS_TBL, UID),
    case Ret of
        [] ->
            true;
        [{_, Path4S3}|_] ->
            catch ets:delete(?LEO_GW_NFS_UID2PATH_ETS_TBL, UID),
            ets:delete(?LEO_GW_NFS_PATH2UID_ETS_TBL, Path4S3)
    end.


%% @doc
get_uid_list() ->
    {ok, lists:flatten(ets:match(?LEO_GW_NFS_UID2PATH_ETS_TBL, {'$1', '_'}))}.


%% @doc
get_path_list() ->
    {ok, lists:flatten(ets:match(?LEO_GW_NFS_UID2PATH_ETS_TBL, {'_', '$1'}))}.


%% @doc
add_readdir_entry(CookieVerf, ReadDirEntry) ->
    ets:insert(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL,
               {CookieVerf, ReadDirEntry}).


%% @doc
get_readdir_entry(CookieVerf) ->
    Ret = ets:lookup(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, CookieVerf),
    case Ret of
        [] ->
            not_found;
        [{_, ReadDirEntry}|_] ->
            {ok, ReadDirEntry}
    end.


%% @doc
del_readdir_entry(CookieVerf) ->
    ets:delete(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, CookieVerf).


%% @doc
add_write_verfier(WriteVerf) ->
    ets:insert(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL,
               {write_verf, WriteVerf}).


%% @doc
get_write_verfier() ->
    Ret = ets:lookup(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, write_verf),
    case Ret of
        [] ->
            not_found;
        [{_, WriteVerf}|_] ->
            {ok, WriteVerf}
    end.


%% ---------------------------------------------------------------------
%% INNER FUNCTIONS
%% ---------------------------------------------------------------------
%% @doc Generates a random binary UUID.
%% @private
v4() ->
    v4(crypto:rand_uniform(1, round(math:pow(2, 48))) - 1,
       crypto:rand_uniform(1, round(math:pow(2, 12))) - 1,
       crypto:rand_uniform(1, round(math:pow(2, 32))) - 1,
       crypto:rand_uniform(1, round(math:pow(2, 30))) - 1).

%% @private
v4(R1, R2, R3, R4) ->
    <<R1:48, 4:4, R2:12, 2:2, R3:32, R4: 30>>.

