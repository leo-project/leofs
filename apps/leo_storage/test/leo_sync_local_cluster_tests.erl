%%====================================================================
%%
%% LeoStorage
%%
%% Copyright (c) 2012-2017 Rakuten, Inc.
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
%%====================================================================
-module(leo_sync_local_cluster_tests).

-include("leo_storage.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_ordning_reda/include/leo_ordning_reda.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

sync_local_cluster_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun suite_regular_1_/1,
                           fun suite_regular_2_/1,
                           fun suite_error_/1
                          ]]}.

setup() ->
    %% prepare network.
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),

    Node = list_to_atom("test_0@" ++ Hostname),
    net_kernel:start([Node, shortnames]),

    %% launch ordning-reda
    leo_ordning_reda_api:start(),
    leo_sync_local_cluster:start_link(Node, 1024, 5000),

    %% mock
    ok = meck:new(leo_redundant_manager_api, [non_strict]),
    ok = meck:expect(leo_redundant_manager_api, get_member_by_node,
                     fun(_Node) ->
                             {ok, #member{state = ?STATE_RUNNING}}
                     end),
    Node.

teardown(Node) ->
    leo_sync_local_cluster:stop(Node),

    net_kernel:stop(),
    meck:unload(),
    ok.

suite_regular_1_(Node) ->
    ok = meck:new(leo_storage_handler_object, [non_strict]),
    ok = meck:expect(leo_storage_handler_object, head, 3, {ok, #?METADATA{clock = -1}}),

    ok = meck:new(leo_object_storage_api, [non_strict]),
    ok = meck:expect(leo_object_storage_api, store,
                     fun(_Metadata, _Object) ->
                             ok
                     end),

    ok = meck:new(leo_misc, [non_strict]),
    ok = meck:expect(leo_misc, get_env,
                     fun(_,_) ->
                             {ok, 12345}
                     end),

    ok = meck:new(leo_watchdog_state, [non_strict]),
    ok = meck:expect(leo_watchdog_state, find_not_safe_items,
                     fun(_) ->
                             not_found
                     end),

    stack(Node),

    %% History = meck:history(leo_object_storage_api),
    %% ?assertEqual(4, length(History)),
    ok.

suite_regular_2_(Node) ->
    ok = meck:new(leo_storage_handler_object, [non_strict]),
    ok = meck:expect(leo_storage_handler_object, head, 2, {ok, #?METADATA{clock = 5}}),

    ok = meck:new(leo_object_storage_api, [non_strict]),
    ok = meck:expect(leo_object_storage_api, store,
                     fun(_Metadata, _Object) ->
                             ok
                     end),

    ok = meck:new(leo_misc, [non_strict]),
    ok = meck:expect(leo_misc, get_env,
                     fun(_,_) ->
                             {ok, 12345}
                     end),

    ok = meck:new(leo_watchdog_state, [non_strict]),
    ok = meck:expect(leo_watchdog_state, find_not_safe_items,
                     fun(_) ->
                             not_found
                     end),
    stack(Node),

    History = meck:history(leo_object_storage_api),
    ?assertEqual([], History),
    ok.

suite_error_(Node) ->
    ok = meck:new(leo_storage_handler_object, [non_strict]),
    ok = meck:expect(leo_storage_handler_object, head, 2, {ok, #?METADATA{clock = -1}}),

    ok = meck:new(leo_object_storage_api, [non_strict]),
    ok = meck:expect(leo_object_storage_api, store,
                     fun(_Metadata, _Object) ->
                             {error, "Not stored"}
                     end),

    ok = meck:new(leo_misc, [non_strict]),
    ok = meck:expect(leo_misc, get_env,
                     fun(_,_) ->
                             {ok, 12345}
                     end),

    ok = meck:new(leo_watchdog_state, [non_strict]),
    ok = meck:expect(leo_watchdog_state, find_not_safe_items,
                     fun(_) ->                             
                             not_found
                     end),
    stack(Node),
    ok.


stack(Node) ->
    AddrId = 1024,
    Size   = 512,
    Key1   = <<"photo/hawaii-0.jpg">>,
    Key2   = <<"photo/hawaii-1.jpg">>,
    Key3   = <<"photo/hawaii-2.jpg">>,
    Key4   = <<"photo/hawaii-3.jpg">>,
    Meta1  = #?METADATA{addr_id = AddrId, key = Key1, dsize = Size, ksize = 18},
    Meta2  = #?METADATA{addr_id = AddrId, key = Key2, dsize = Size, ksize = 18},
    Meta3  = #?METADATA{addr_id = AddrId, key = Key3, dsize = Size, ksize = 18},
    Meta4  = #?METADATA{addr_id = AddrId, key = Key4, dsize = Size, ksize = 18},
    Object = crypto:strong_rand_bytes(Size),

    _ = leo_sync_local_cluster:stack([Node], AddrId, Key1, Meta1, Object),
    _ = leo_sync_local_cluster:stack([Node], AddrId, Key2, Meta2, Object),
    _ = leo_sync_local_cluster:stack([Node], AddrId, Key3, Meta3, Object),
    _ = leo_sync_local_cluster:stack([Node], AddrId, Key4, Meta4, Object),
    ok.

-endif.

