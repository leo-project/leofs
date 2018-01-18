%%======================================================================
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
%%
%% ---------------------------------------------------------------------
%% LeoFS Storage - EUnit
%% @doc
%% @end
%%======================================================================
-module(leo_storage_handler_object_tests).

-include("leo_storage.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

-define(TEST_BUCKET, <<"air">>).
-define(TEST_DIR_0,  <<"air/on/g/">>).
-define(TEST_KEY_0,  <<"air/on/g/string">>).
-define(TEST_KEY_1,  <<"air/on/g/bach/music">>).
-define(TEST_BIN,    <<"V">>).
-define(TEST_META_0, #?METADATA{key   = ?TEST_KEY_0,
                                dsize = byte_size(?TEST_BIN)}).
-define(TEST_META_1, #?METADATA{key   = ?TEST_KEY_0,
                                dsize = byte_size(?TEST_BIN),
                                del   = 1
                               }).


object_handler_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun get_a0_/1,
                           fun get_a1_/1,
                           fun get_nodedown/1,
                           fun get_unavailablenode/1,
                           %% fun get_b0_/1,
                           %% fun get_b1_/1,
                           %% fun get_b2_/1,
                           %% fun get_b3_/1,
                           %% fun get_c0_/1,
                           fun put_0_/1,
                           fun put_1_/1,
                           fun delete_/1,
                           fun head_/1,
                           fun copy_/1,
                           fun prefix_search_/1,
                           fun prefix_search_and_remove_objects_/1
                          ]]}.

setup() ->
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),

    Node0 = list_to_atom("node_0@" ++ Hostname),
    net_kernel:start([Node0, shortnames]),
    {ok, Node1} = slave:start_link(list_to_atom(Hostname), 'node_1'),

    true = rpc:call(Node0, code, add_path, ["../deps/meck/ebin"]),
    true = rpc:call(Node1, code, add_path, ["../deps/meck/ebin"]),

    catch leo_logger_api:new("./", ?LOG_LEVEL_WARN),
    catch leo_logger_api:new(?LOG_GROUP_ID_ACCESS, ?LOG_ID_ACCESS,
                             "./", ?LOG_FILENAME_ACCESS),
    {Node0, Node1}.

teardown({_, Node1}) ->
    meck:unload(),
    net_kernel:stop(),
    slave:stop(Node1),
    leo_logger_api:stop(),
    timer:sleep(100),
    ok.

%%--------------------------------------------------------------------
%% GET
%%--------------------------------------------------------------------
%% @doc  get/1
%% @private
get_a0_({Node0, Node1}) ->
    %% leo_redundant_manager_api
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(get, _Key) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [{Node0,true}, {Node1,true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    %% leo_object_storage_api
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        not_found
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        not_found
                end),

    meck:new(leo_statistics_req_counter, [non_strict]),
    meck:expect(leo_statistics_req_counter, increment,
                fun(_) -> ok end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    Ref = make_ref(),
    Res = leo_storage_handler_object:get({Ref, ?TEST_KEY_0}),
    ?assertEqual({error, Ref, not_found}, Res),
    ok.

%% @doc  get/1
%% @private
get_a1_({Node0, Node1}) ->
    %% leo_redundant_manager_api
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [{Node0,true}, {Node1,true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    %% leo_object_storage_api
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        {ok, ?TEST_META_0, #?OBJECT{}}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        {ok, ?TEST_META_0, #?OBJECT{}}
                end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    meck:expect(leo_storage_read_repairer, repair,
                fun(_,_,_,_) -> {ok, ?TEST_META_0, <<>>} end),

    Ref = make_ref(),
    Res = leo_storage_handler_object:get({Ref, ?TEST_KEY_0}),
    ?assertEqual({ok, Ref, ?TEST_META_0, <<>>}, Res),
    ok.

%% @doc  Get Test when One Node is up but another is down, up node does not hold the object
%% @@ private
get_nodedown({Node0, Node1}) ->
    %% leo_redundant_manager_api
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    %% leo_object_storage_api
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        {error, not_found}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        {error, not_found}
                end),

    meck:new(rpc, [unstick, passthrough]),
    meck:expect(rpc, nb_yield,
                fun(_, _) ->
                        {value, {badrpc, nodedown}}
                end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    meck:new(leo_storage_watchdog_error, [non_strict]),
    meck:expect(leo_storage_watchdog_error, push, fun(_) -> ok end),

    AddrId = 0,
    ReqId = 0,
    Res = leo_storage_handler_object:get(AddrId, ?TEST_KEY_0, ReqId),
    ?assertNotEqual({error, not_found}, Res),
    ok.

get_unavailablenode({Node0, Node1}) ->
    %% leo_redundant_manager_api
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = false}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    %% leo_object_storage_api
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        {error, not_found}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        {error, not_found}
                end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    meck:new(leo_storage_watchdog_error, [non_strict]),
    meck:expect(leo_storage_watchdog_error, push, fun(_) -> ok end),

    AddrId = 0,
    ReqId = 0,
    Res = leo_storage_handler_object:get(AddrId, ?TEST_KEY_0, ReqId),
    ?assertNotEqual({error, not_found}, Res),
    ok.

%%--------------------------------------------------------------------
%% PUT
%%--------------------------------------------------------------------
%% put/6
put_0_({Node0, Node1}) ->
    AddrId    = 0,
    Key       = ?TEST_KEY_0,
    Bin       = ?TEST_BIN,
    Size      = byte_size(?TEST_BIN),
    ReqId     = 0,
    Timestamp = 0,

    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(put, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    meck:new(leo_storage_replicator, [non_strict]),
    meck:expect(leo_storage_replicator, replicate,
                fun(_Method,_Quorum,_Redundancies,_ObjectPool,_Callback) ->
                        {ok, {etag, 1}}
                end),

    ok = rpc:call(Node1, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    Object = #?OBJECT{method    = ?CMD_PUT,
                      addr_id   = AddrId,
                      key       = Key,
                      data      = Bin,
                      dsize     = Size,
                      req_id    = ReqId,
                      timestamp = Timestamp,
                      del       = 0},
    {ok, _Checksum} = leo_storage_handler_object:put(Object, 0),
    ok.

%% put/2
put_1_({_Node0, _Node1}) ->
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, put,
                fun(_Key, _ObjPool) ->
                        {ok, 1}
                end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    Ref = make_ref(),
    {ok, Ref, _Etag} = leo_storage_handler_object:put({#?OBJECT{}, Ref}),
    ok.


%%--------------------------------------------------------------------
%% DELETE
%%--------------------------------------------------------------------
%% delete/4
delete_({Node0, Node1}) ->
    AddrId    = 0,
    Key       = ?TEST_KEY_0,
    ReqId     = 0,
    Timestamp = 0,

    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(put, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),
    meck:expect(leo_redundant_manager_api, get_members_by_status,
                fun(_State) ->
                        not_found
                end),

    meck:new(leo_storage_replicator, [non_strict]),
    meck:expect(leo_storage_replicator, replicate,
                fun(_Method,_Quorum,_Redundancies,_ObjectPool,_Callback) ->
                        {ok, 0}
                end),

    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, fetch_by_key,
                fun(_ParentDir,_) ->
                        ?debugVal(_ParentDir),
                        {ok, []}
                end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items,
                fun(_) ->
                        not_found
                end),

    meck:new(leo_storage_mq, [non_strict]),
    meck:expect(leo_storage_mq, publish,
                fun(_,_) ->
                        ok
                end),
    meck:new(leo_storage_handler_del_directory, [non_strict]),
    meck:expect(leo_storage_handler_del_directory, enqueue,
                fun(_) ->
                        ok
                end),
    meck:expect(leo_storage_handler_del_directory, enqueue,
                fun(_,_) ->
                        ok
                end),
    meck:expect(leo_storage_handler_del_directory, enqueue,
                fun(_,_,_) ->
                        ok
                end),

    ok = rpc:call(Node1, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    Object_1 = #?OBJECT{method    = ?CMD_DELETE,
                        addr_id   = AddrId,
                        key       = << Key/binary, "/" >>,
                        data      = <<>>,
                        dsize     = 0,
                        req_id    = ReqId,
                        timestamp = Timestamp,
                        del       = 1},
    Res_1 = leo_storage_handler_object:delete(Object_1, 0),
    ?assertEqual(ok, Res_1),

    Object_2 = #?OBJECT{method    = ?CMD_DELETE,
                        addr_id   = AddrId,
                        key       = << Key/binary >>,
                        data      = <<>>,
                        dsize     = 0,
                        req_id    = ReqId,
                        timestamp = Timestamp,
                        del       = 1},
    Res_2 = leo_storage_handler_object:delete(Object_2, 0),
    ?assertEqual(ok, Res_2),
    ok.


%%--------------------------------------------------------------------
%% OTHER
%%--------------------------------------------------------------------
head_({Node0, Node1}) ->
    %% 1.
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun(_Key) ->
                        {ok, term_to_binary(?TEST_META_0)}
                end),
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    Res0 = leo_storage_handler_object:head(0, ?TEST_KEY_0),
    ?assertEqual({ok, ?TEST_META_0}, Res0),

    %% 2.
    meck:unload(),
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun(_Key) ->
                        not_found
                end),
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    Res1 = leo_storage_handler_object:head(0, ?TEST_KEY_0),
    ?assertEqual({error, not_found}, Res1),

    %% 2.
    meck:unload(),
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = false},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    ok = rpc:call(Node1, meck, new,    [leo_object_storage_api, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_object_storage_api, head,
                                        fun(_Arg) ->
                                                {ok, term_to_binary(?TEST_META_0)}
                                        end]),

    {ok, Res2} = leo_storage_handler_object:head(0, ?TEST_KEY_0),
    ?assertEqual(?TEST_META_0, Res2),
    ok.


copy_({Node0, Node1}) ->
    %% 1. for WRITE
    %%
    %% Retrieve metadata from head-func
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun(_Key) ->
                        {ok, term_to_binary(?TEST_META_0)}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        {ok, ?TEST_META_0, #?OBJECT{}}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        {ok, ?TEST_META_0, #?OBJECT{}}
                end),

    %% Retrieve object from get-func
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(get, _AddrId) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [{Node0,true}, {Node1,true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),
    meck:expect(leo_redundant_manager_api, get_member_by_node,
                fun(_) ->
                        {ok, #member{state = ?STATE_RUNNING}}
                end),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(_) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [{Node0,true}, {Node1,true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    %% ording-reda
    meck:new(leo_ordning_reda_api, [non_strict]),
    meck:expect(leo_ordning_reda_api, add_container,
                fun(_,_,_) ->
                        ok
                end),
    meck:expect(leo_ordning_reda_api, stack,
                fun(_,_,_) ->
                        ok
                end),
    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    Res1 = leo_storage_handler_object:replicate([Node1, Node1], 0, ?TEST_KEY_0),
    ?assertEqual(ok, Res1),

    %% 2. for DELETE
    %%
    meck:unload(leo_object_storage_api),
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun(_Key) ->
                        {ok, term_to_binary(?TEST_META_1)}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos) ->
                        {ok, ?TEST_META_1, #?OBJECT{}}
                end),
    meck:expect(leo_object_storage_api, get,
                fun(_Key, _StartPos, _EndPos, _IsForcedCheck) ->
                        {ok, ?TEST_META_1, #?OBJECT{}}
                end),

    Res2 = leo_storage_handler_object:replicate([Node1, Node1], 0, ?TEST_KEY_0),
    ?assertEqual(ok, Res2),
    ok.


prefix_search_({_Node0, _Node1}) ->
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, fetch_by_key,
                fun(_ParentDir, Fun) ->
                        Fun(?TEST_KEY_0, term_to_binary(#?METADATA{}), []),
                        Fun(?TEST_KEY_1, term_to_binary(#?METADATA{}), [#?METADATA{key=?TEST_KEY_0}])
                end),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun(_) -> not_found end),

    Res = leo_storage_handler_object:prefix_search(?TEST_DIR_0, [], 1000),
    ?assertEqual(2, length(Res)),
    ok.

prefix_search_and_remove_objects_(_) ->
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, fetch_by_key_in_parallel,
                fun(_ParentDir, Fun,_) ->
                        Fun(?TEST_KEY_0, term_to_binary(#?METADATA{}), []),
                        Fun(?TEST_DIR_0, term_to_binary(#?METADATA{}), [#?METADATA{key=?TEST_KEY_0}]),
                        Fun(?TEST_KEY_1, term_to_binary(#?METADATA{}), [#?METADATA{key=?TEST_KEY_0}]),
                        Fun(<< "_", ?TEST_KEY_1/binary >>,
                            term_to_binary(#?METADATA{}), [#?METADATA{key=?TEST_KEY_0}])
                end),

    meck:new(leo_mq_api, [non_strict]),
    meck:expect(leo_mq_api, publish, fun(_,_,_) -> ok end),
    meck:expect(leo_mq_api, publish, fun(_,_,_,_) -> ok end),

    Res = leo_storage_handler_object:prefix_search_and_remove_objects(
            mqid, ?TEST_BUCKET, leo_date:clock()),
    ?assertEqual(true, is_list(Res)),
    ok.

-endif.
