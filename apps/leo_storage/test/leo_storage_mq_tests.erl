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
%%
%% -------------------------------------------------------------------
%% LeoFS Storage - EUnit
%% @doc
%% @end
%%====================================================================
-module(leo_storage_mq_tests).

-include("leo_storage.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_VNODE_ID, 1024).
-define(TEST_KEY_1, <<"air/on/g/string/music.png">>).

-define(TEST_MSG_1, term_to_binary(#inconsistent_data_message{id         = 12345,
                                                              type       = ?ERR_TYPE_REPLICATE_DATA,
                                                              key        = ?TEST_KEY_1,
                                                              timestamp  = 20110228100000})).
-define(TEST_MSG_2, term_to_binary(#sync_unit_of_vnode_message{id        = 12345,
                                                               vnode_id  = 256,
                                                               timestamp = 20110228100000})).
-define(TEST_MSG_3, term_to_binary(#rebalance_message{id       = 12345,
                                                      node     = 'test_1@127.0.0.1',
                                                      vnode_id = ?TEST_VNODE_ID,
                                                      addr_id  = ?TEST_VNODE_ID,
                                                      key      = ?TEST_KEY_1})).

-define(TEST_META_1, #?METADATA{key       = ?TEST_KEY_1,
                                addr_id   = 1,
                                clock     = 9,
                                timestamp = 8,
                                checksum  = 7}).
-define(TEST_META_2, #?METADATA{key       = ?TEST_KEY_1,
                                addr_id   = 1,
                                clock     = 8,
                                timestamp = 7,
                                checksum  = 5}).

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

mq_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun start_/1,
                           fun publish_/1,
                           fun subscribe_0_/1,
                           fun subscribe_1_/1,
                           fun subscribe_2_/1,
                           fun subscribe_3_/1
                          ]]}.

setup() ->
    %% prepare network.
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),

    Test0Node = list_to_atom("test_0@" ++ Hostname),
    net_kernel:start([Test0Node, shortnames]),
    {ok, Test1Node} = slave:start_link(list_to_atom(Hostname), 'test_1'),

    true = rpc:call(Test0Node, code, add_path, ["../deps/meck/ebin"]),
    true = rpc:call(Test1Node, code, add_path, ["../deps/meck/ebin"]),

    %% gen mock.
    %% meck:new(leo_logger_api, [non_strict]),
    %% meck:expect(leo_logger_api, new,          fun(_,_,_) -> ok end),
    %% meck:expect(leo_logger_api, new,          fun(_,_,_,_,_) -> ok end),
    %% meck:expect(leo_logger_api, new,          fun(_,_,_,_,_,_) -> ok end),
    %% meck:expect(leo_logger_api, add_appender, fun(_,_) -> ok end),
    %% meck:expect(leo_logger_api, append,       fun(_,_) -> ok end),
    %% meck:expect(leo_logger_api, append,       fun(_,_,_) -> ok end),

    meck:new(leo_mq_api, [non_strict]),
    meck:expect(leo_mq_api, new,     fun(_,_,_) -> ok end),
    meck:expect(leo_mq_api, publish, fun(_,_,_) -> ok end),

    meck:new(leo_mq_logger, [non_strict]),
    meck:expect(leo_mq_logger, append, fun(_MQ) -> ok end),

    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(_, _) ->
                        {ok, #redundancies{nodes = [#redundant_node{node = Test0Node,
                                                                    available = true},
                                                    #redundant_node{node = Test1Node,
                                                                    available = true}],
                                           id = ?TEST_VNODE_ID}}
                end),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(_) ->
                        {ok, #redundancies{nodes = [#redundant_node{node = Test0Node,
                                                                    available = true},
                                                    #redundant_node{node = Test1Node,
                                                                    available = true}],
                                           id = ?TEST_VNODE_ID}}
                end),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(_,_) ->
                        {ok, #redundancies{nodes = []}}
                end),

    meck:expect(leo_redundant_manager_api, range_of_vnodes,
                fun(_) ->
                        {ok,[{340121982302782409338486978520692208515,
                              340282366920938463463374607431768211456},
                             {0,21732878282644261548789301125521339}]}
                end),
    meck:expect(leo_redundant_manager_api, checksum,
                fun(_) ->
                        {ok,{12345,56789}}
                end),

    meck:new(leo_storage_handler_del_directory, [non_strict]),
    meck:expect(leo_storage_handler_del_directory, get_cached_items,
                fun() ->
                        {ok, []}
                end),

    meck:new(leo_logger_api, [non_strict]),
    meck:expect(leo_logger_api, error,
                fun(_) ->
                        ok
                end),
    meck:expect(leo_logger_api, warn,
                fun(_) ->
                        ok
                end),
    {Test0Node, Test1Node}.

teardown({_Test0Node, Test1Node}) ->
    meck:unload(),

    net_kernel:stop(),
    slave:stop(Test1Node),
    timer:sleep(100),
    ok.


start_(_) ->
    %% @TODO
    %% RefSup = case whereis(leo_storage_sup) of
    %%              undefined ->
    %%                  {ok, Pid} = leo_storage_sup:start_link(),
    %%                  Pid;
    %%              Pid ->
    %%                  Pid
    %%          end,
    %% ok = leo_storage_mq:start(RefSup, "queue"),
    %% Res = meck:history(leo_mq_api),
    %% ?assertEqual(4, length(Res)),
    ok.

%% sync vnode-id queue.
publish_({_, Test1Node}) ->
    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER, [named_table, public]),
    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun({_AddrId, _Key}) ->
                        {ok, term_to_binary(#?METADATA{num_of_replicas = 0})}
                end),

    ok = leo_storage_mq:publish(?QUEUE_ID_RECOVERY_NODE, node()),
    ok = leo_storage_mq:publish(?QUEUE_ID_ASYNC_DELETION, #?METADATA{addr_id = ?TEST_VNODE_ID,
                                                                     key = ?TEST_KEY_1}),
    ok = leo_storage_mq:publish(?QUEUE_ID_ASYNC_DELETION, ?TEST_VNODE_ID, ?TEST_KEY_1),

    ok = leo_storage_mq:publish(?QUEUE_ID_SYNC_BY_VNODE_ID, ?TEST_VNODE_ID, node()),
    ok = leo_storage_mq:publish(?QUEUE_ID_SYNC_OBJ_WITH_DC, ?TEST_VNODE_ID, ?TEST_KEY_1),
    ok = leo_storage_mq:publish(?QUEUE_ID_COMP_META_WITH_DC, 'leofs_1', [{?TEST_VNODE_ID, ?TEST_KEY_1}]),
    ok = leo_storage_mq:publish(?QUEUE_ID_REQ_DEL_DIR, node(), <<"leofs/">>),
    ok = leo_storage_mq:publish({?QUEUE_ID_DEL_DIR, 'worker_1'}, ?TEST_VNODE_ID, ?TEST_KEY_1, leo_date:clock()),
    ok = leo_storage_mq:publish(?QUEUE_ID_SYNC_OBJ_WITH_DC, 'leofs_1', ?TEST_VNODE_ID, ?TEST_KEY_1),

    ok = leo_storage_mq:publish(
           ?QUEUE_ID_SYNC_BY_VNODE_ID, ?TEST_VNODE_ID, Test1Node),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_REBALANCE, Test1Node, ?TEST_VNODE_ID, ?TEST_VNODE_ID, ?TEST_KEY_1),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, #?METADATA{addr_id = ?TEST_VNODE_ID,
                                            key = ?TEST_KEY_1}, ?ERR_TYPE_REPLICATE_DATA),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, ?TEST_VNODE_ID, ?TEST_KEY_1, ?ERR_TYPE_REPLICATE_DATA),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, ?TEST_VNODE_ID, ?TEST_KEY_1, ?ERR_TYPE_DELETE_DATA),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, ?TEST_VNODE_ID, ?TEST_KEY_1, ?ERR_TYPE_REPLICATE_INDEX),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, ?TEST_VNODE_ID, ?TEST_KEY_1, ?ERR_TYPE_DELETE_INDEX),
    ok = leo_storage_mq:publish(
           ?QUEUE_ID_PER_OBJECT, ?TEST_VNODE_ID, ?TEST_KEY_1, ?ERR_TYPE_RECOVER_DATA),

    History0 = meck:history(leo_mq_api),
    ?assertEqual(true, length(History0) > 0),

    true = ets:delete(?TBL_REBALANCE_COUNTER),
    ok.


%% miss-replication queue -> subscribe.
subscribe_0_({Test0Node, Test1Node}) ->
    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER, [named_table, public]),

    %% case-1.
    ok = rpc:call(Test0Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test0Node, meck, expect, [leo_storage_handler_object, head,
                                            fun(_Key, _VNodeId) ->
                                                    {ok, ?TEST_META_1}
                                            end]),
    ok = rpc:call(Test1Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_storage_handler_object, head,
                                            fun(_Key, _VNodeId) ->
                                                    {ok, ?TEST_META_1}
                                            end]),
    meck:expect(leo_redundant_manager_api, get_member_by_node,
                fun(_Node) ->
                        {ok, #member{state = ?STATE_RUNNING}}
                end),

    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun({_AddrId, _Key}) ->
                        {ok, term_to_binary(#?METADATA{num_of_replicas = 0})}
                end),

    timer:sleep(100),
    leo_storage_mq:handle_call({consume, ?QUEUE_ID_PER_OBJECT, ?TEST_MSG_1}),

    true = ets:delete(?TBL_REBALANCE_COUNTER),
    ok.

%% miss-replication queue -> subscribe.
subscribe_1_({Test0Node, Test1Node}) ->
    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER, [named_table, public]),

    %% case-1.
    ok = rpc:call(Test0Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test0Node, meck, expect, [leo_storage_handler_object, head,
                                            fun(_Key, _VNodeId, _DoRetry) ->
                                                    {ok, ?TEST_META_1}
                                            end]),
    ok = rpc:call(Test1Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_storage_handler_object, head,
                                            fun(_Key, _VNodeId, _DoRetry) ->
                                                    {ok, ?TEST_META_2}
                                            end]),

    ok = rpc:call(Test0Node, meck, new,    [leo_storage_api, [no_link, non_strict]]),
    ok = rpc:call(Test0Node, meck, expect, [leo_storage_api, synchronize,
                                            fun(_InconsistentNodes, _CorrectMetadata) ->
                                                    ok
                                            end]),
    ok = rpc:call(Test1Node, meck, new,    [leo_object_storage_api, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_object_storage_api, head_with_check_avs,
                                            fun(_AddrIdAndKey, _CheckMethod) ->
                                                    {error, invalid_object}
                                            end]),
    meck:expect(leo_redundant_manager_api, get_member_by_node,
                fun(_Node) ->
                        {ok, #member{state = ?STATE_RUNNING}}
                end),

    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun({_AddrId, _Key}) ->
                        {ok, term_to_binary(#?METADATA{num_of_replicas = 0})}
                end),
    meck:expect(leo_object_storage_api, head_with_check_avs,
                fun({_AddrId, _Key}, _CheckMethod) ->
                        {ok, term_to_binary(#?METADATA{num_of_replicas = 0})}
                end),

    timer:sleep(100),
    leo_storage_mq:handle_call({consume, ?QUEUE_ID_PER_OBJECT, ?TEST_MSG_1}),

    History1 = rpc:call(Test0Node, meck, history, [leo_storage_api]),
    ?assertEqual(1, length(History1)),

    true = ets:delete(?TBL_REBALANCE_COUNTER),
    ok.


subscribe_2_({Test0Node, _Test1Node}) ->
    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER, [named_table, public]),

    meck:new(leo_storage_handler_object, [non_strict]),
    meck:expect(leo_storage_handler_object, copy,
                fun(_,_,_) ->
                        ok
                end),
    meck:expect(leo_storage_handler_object, replicate,
                fun(_,_,_) ->
                        ok
                end),
    meck:expect(leo_storage_handler_object, get,
                fun({_Ref,_}) ->
                        {ok,_Ref, <<>>, <<>>}
                end),
    meck:expect(leo_storage_handler_object, is_key_under_del_dir,
                fun(_) ->
                        true
                end),

    meck:expect(leo_redundant_manager_api, get_member_by_node,
                fun(_Node) ->
                        {ok, #member{state = ?STATE_RUNNING}}
                end),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_addr_id,
                fun(_,_) ->
                        {ok, #redundancies{nodes = [{Test0Node, true}]}}
                end),

    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, head,
                fun({_AddrId, _Key}) ->
                        {ok, term_to_binary(#?METADATA{num_of_replicas = 0})}
                end),

    timer:sleep(100),

    leo_storage_mq:handle_call(
      {consume, ?QUEUE_ID_REBALANCE, ?TEST_MSG_3}),

    true = ets:delete(?TBL_REBALANCE_COUNTER),
    ok.


%% miss-replication queue -> subscribe.
subscribe_3_({_Test0Node, _Test1Node}) ->
    ?TBL_REBALANCE_COUNTER = ets:new(?TBL_REBALANCE_COUNTER, [named_table, public]),

    meck:new(leo_object_storage_api, [non_strict]),
    meck:expect(leo_object_storage_api, fetch_by_addr_id,
                fun(_FromVNodeId, Fun) ->
                        Fun(<<"key">>, term_to_binary(#?METADATA{ring_hash = 0}), []),
                        ok
                end),
    meck:expect(leo_redundant_manager_api, get_member_by_node,
                fun(_Node) ->
                        {ok, #member{state = ?STATE_RUNNING}}
                end),

    timer:sleep(100),
    leo_storage_mq:handle_call({consume, ?QUEUE_ID_SYNC_BY_VNODE_ID, ?TEST_MSG_2}),

    [{_, {leo_object_storage_api,fetch_by_addr_id,
          [340121982302782409338486978520692208515,_]}, ok},
     {_, {leo_object_storage_api,fetch_by_addr_id,
          [0,_]}, ok}] = meck:history(leo_object_storage_api),

    true = ets:delete(?TBL_REBALANCE_COUNTER),
    ok.

-endif.
