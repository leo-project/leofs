%%====================================================================
%%
%% LeoFS Gateway
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
%% LeoFS Gateway - RPC Handler Test
%% @doc
%% @end
%%====================================================================
-module(leo_nfs_readdir_state_ets_tests).

-include("leo_gateway.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_COOKIE, <<1,2,3,4,5,6,7,8>>).

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
readdir_test_() ->
    {setup, fun setup_readdir_state_ets/0,
     [{timeout, 30, fun readdir_entry_and_delete/0},
      {timeout, 30, fun readdir_entry_and_clean/0}]}.

get_count() ->
    Info = leo_nfs_readdir_state_ets:info(),
    proplists:get_value('size', Info).

setup_readdir_state_ets() ->
    leo_nfs_readdir_state_ets:start_link(
      [{nfsd_readdir_scan_int, 3},
       {nfsd_readdir_entry_ttl, 10}]),
    ok.

readdir_entry_and_delete() ->
    leo_nfs_readdir_state_ets:add_readdir_entry(?TEST_COOKIE, dummy),
    Cnt_1 = get_count(),
    ?assertEqual(1, Cnt_1),
    Ret = leo_nfs_readdir_state_ets:get_readdir_entry(?TEST_COOKIE),
    ?assertMatch({ok, _}, Ret),
    leo_nfs_readdir_state_ets:del_readdir_entry(?TEST_COOKIE),
    Cnt_2 = get_count(),
    ?assertEqual(0, Cnt_2),
    ok.

readdir_entry_and_clean() ->
    leo_nfs_readdir_state_ets:add_readdir_entry(?TEST_COOKIE, dummy),
    Cnt_1 = get_count(),
    ?assertEqual(1, Cnt_1),
    timer:sleep(5000),
    Cnt_2 = get_count(),
    ?assertEqual(1, Cnt_2),
    timer:sleep(10000),
    Cnt_3 = get_count(),
    ?assertEqual(0, Cnt_3),
    ok.

-endif.
