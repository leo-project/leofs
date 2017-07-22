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
-module(leo_storage_read_repairer_tests).

-include("leo_storage.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").

-define(TEST_KEY_1, <<"air/on/g/string/music.png">>).
-define(TEST_META_1, #?METADATA{key       = ?TEST_KEY_1,
                                addr_id   = 1,
                                clock     = 9,
                                timestamp = 8,
                                checksum  = 7}).
-define(TEST_META_2, #?METADATA{key       = ?TEST_KEY_1,
                                addr_id   = 1,
                                clock     = 7,
                                timestamp = 8,
                                checksum  = 7}).

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

read_repair_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun regular_/1,
                           fun fail_1_/1,
                           fun fail_2_/1
                          ]]}.

setup() ->
    meck:new(leo_logger, [non_strict]),
    meck:expect(leo_logger, append, fun(_,_,_) ->
                                            ok
                                    end),

    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),

    Test0Node = list_to_atom("test_0@" ++ Hostname),
    net_kernel:start([Test0Node, shortnames]),
    {ok, Test1Node} = slave:start_link(list_to_atom(Hostname), 'test_1'),

    true = rpc:call(Test0Node, code, add_path, ["../deps/meck/ebin"]),
    true = rpc:call(Test1Node, code, add_path, ["../deps/meck/ebin"]),

    timer:sleep(100),
    {Test0Node, Test1Node}.


teardown({_Test0Node, Test1Node}) ->
    meck:unload(),
    net_kernel:stop(),
    slave:stop(Test1Node),
    ok.


regular_({Test0Node, Test1Node}) ->
    meck:new(leo_storage_handler_object, [non_strict]),
    meck:expect(leo_storage_handler_object, head, fun(_,_) ->
                                                          {ok, ?TEST_META_1}
                                                  end),
    ok = rpc:call(Test1Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_storage_handler_object, head, fun(_, _) ->
                                                                                      {ok, ?TEST_META_1}
                                                                              end]),
    meck:new(leo_storage_mq, [non_strict]),
    meck:expect(leo_storage_mq, publish,
                fun(_,_,_) ->
                        ok
                end),

    Nodes = [#redundant_node{node = Test0Node, available = true},
             #redundant_node{node = Test1Node, available = true}],
    F = fun(_) -> ok end,
    ok = leo_storage_read_repairer:repair(
           #read_parameter{quorum = 1,
                           req_id = 0}, Nodes, ?TEST_META_1, F),
    ok.

fail_1_({Test0Node, Test1Node}) ->
    meck:new(leo_storage_handler_object, [non_strict]),
    meck:expect(leo_storage_handler_object, head, fun(_,_) ->
                                                          {ok, ?TEST_META_1}
                                                  end),
    ok = rpc:call(Test1Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_storage_handler_object, head, fun(_, _) ->
                                                                                      {ok, ?TEST_META_2}
                                                                              end]),
    meck:new(leo_storage_mq, [non_strict]),
    meck:expect(leo_storage_mq, publish,
                fun(_,_,_) ->
                        ok
                end),

    Nodes = [#redundant_node{node = Test0Node, available = true},
             #redundant_node{node = Test1Node, available = true}],
    F = fun(_) -> ok end,
    ok = leo_storage_read_repairer:repair(
           #read_parameter{quorum = 1,
                           req_id = 0}, Nodes, ?TEST_META_1, F),
    ok.

fail_2_({Test0Node, Test1Node}) ->
    meck:new(leo_storage_handler_object, [non_strict]),
    meck:expect(leo_storage_handler_object, head, fun(_,_) ->
                                                          {ok, ?TEST_META_2}
                                                  end),
    ok = rpc:call(Test1Node, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Test1Node, meck, expect, [leo_storage_handler_object, head, fun(_, _) ->
                                                                                      {ok, ?TEST_META_1}
                                                                              end]),
    meck:new(leo_storage_mq, [non_strict]),
    meck:expect(leo_storage_mq, publish,
                fun(_,_,_) ->
                        ok
                end),

    Nodes = [#redundant_node{node = Test0Node, available = true},
             #redundant_node{node = Test1Node, available = true}],
    F = fun(_) -> ok end,
    ok = leo_storage_read_repairer:repair(
           #read_parameter{quorum = 1,
                           req_id = 0}, Nodes, ?TEST_META_2, F),
    ok.

-endif.
