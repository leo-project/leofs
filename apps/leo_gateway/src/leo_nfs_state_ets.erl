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
         add_write_verfier/1, get_write_verfier/0]).


-define(LEO_GW_NFS_UID2PATH_ETS_TBL, uid2path_ets_tbl).
-define(LEO_GW_NFS_PATH2UID_ETS_TBL, path2uid_ets_tbl).
-define(LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL, write_verifier_ets_tbl).
-define(LEO_GW_NFS_FILEID_GEN_TBL, fileid_gen_ets_tbl).
-define(LEO_GW_NFS_FILEID_GEN_KEY, counter).


%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------
%% @doc
init(_Params) ->
    ets:new(?LEO_GW_NFS_UID2PATH_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_PATH2UID_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL, [set, named_table, public]),
    ets:new(?LEO_GW_NFS_FILEID_GEN_TBL, [set, named_table, public]),
    ets:insert(?LEO_GW_NFS_FILEID_GEN_TBL, {?LEO_GW_NFS_FILEID_GEN_KEY, 0}),
    ok.


%% @doc
add_path(Path4S3) ->
    Ret = ets:lookup(?LEO_GW_NFS_PATH2UID_ETS_TBL, Path4S3),
    {ok, UID_1} = case Ret of
                      [] ->
                          NewUID = uuid(),
                          ets:insert(?LEO_GW_NFS_UID2PATH_ETS_TBL, {NewUID, Path4S3}),
                          ets:insert(?LEO_GW_NFS_PATH2UID_ETS_TBL, {Path4S3, NewUID}),
                          {ok, NewUID};
                      [{_, UID}|_] ->
                          %% already exists
                          {ok, UID}
                  end,
    {ok, UID_1}.


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
add_write_verfier(WriteVerf) ->
    ets:insert(?LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL,
               {write_verf, WriteVerf}).


%% @doc
get_write_verfier() ->
    Ret = ets:lookup(?LEO_GW_NFS_WRITE_VERIFIER_ETS_TBL, write_verf),
    case Ret of
        [] ->
            not_found;
        [{_, WriteVerf}|_] ->
            {ok, WriteVerf}
    end.


%% ---------------------------------------------------------------------
%% INNER FUNCTIONS
%% ---------------------------------------------------------------------
%% @doc Generates a binary UUID by atomic incremental counter.
%% @private
uuid() ->
    ID = ets:update_counter(?LEO_GW_NFS_FILEID_GEN_TBL, ?LEO_GW_NFS_FILEID_GEN_KEY, 1),
    leo_hex:integer_to_raw_binary(ID, 16).
