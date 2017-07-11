%%======================================================================
%%
%% Leo Manager
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
-module(leo_manager_mnesia_tests).

-include("leo_manager.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_libs.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

manager_mnesia_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun all_/1
                          ]]}.

setup() ->
    application:start(mnesia),
    ok.

teardown(_) ->
    application:stop(mnesia),
    ok.

%%--------------------------------------------------------------------
%%% TEST FUNCTIONS
%%--------------------------------------------------------------------
all_(_) ->
    %% create tables
    ok = leo_cluster_tbl_conf:create_table(ram_copies, [node()]),
    ok = leo_manager_mnesia:create_storage_nodes(ram_copies, [node()]),
    ok = leo_manager_mnesia:create_gateway_nodes(ram_copies, [node()]),
    ok = leo_manager_mnesia:create_rebalance_info(ram_copies, [node()]),
    ok = leo_manager_mnesia:create_available_commands(ram_copies, [node()]),
    ok = leo_manager_mnesia:create_del_bucket_state(ram_copies, [node()]),
    ok = leo_s3_auth:create_table(ram_copies, [node()]),
    ok = leo_s3_bucket:create_table(ram_copies, [node()]),
    ok = leo_s3_endpoint:create_table(ram_copies, [node()]),
    ok = leo_s3_user:create_table(ram_copies, [node()]),
    ok = leo_s3_user_credential:create_table(ram_copies, [node()]),

    ?assertEqual(true, length(mnesia:system_info(tables)) > 1),

    %% get-1 > not_found
    not_found = leo_manager_mnesia:get_storage_nodes_all(),
    not_found = leo_manager_mnesia:get_storage_node_by_name(node()),
    not_found = leo_manager_mnesia:get_storage_nodes_by_status('running'),
    not_found = leo_manager_mnesia:get_gateway_nodes_all(),
    not_found = leo_manager_mnesia:get_gateway_node_by_name(node()),
    not_found = leo_manager_mnesia:get_rebalance_info_all(),
    not_found = leo_manager_mnesia:get_rebalance_info_by_node(node()),

    %%
    %% put/get
    %%
    %% storage-node
    Node0  = 'test0@127.0.0.1',
    State0 = 'running',
    NodeState0 = #node_state{node  = Node0,
                             state = State0},

    ok = leo_manager_mnesia:update_storage_node_status(NodeState0),
    Res0 = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual({ok,[{node_state,'test0@127.0.0.1',undefined,"-1","-1",0,0}]}, Res0),

    ok = leo_manager_mnesia:update_storage_node_status(update, NodeState0),
    {ok, Res1} = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual([#node_state{node = Node0,
                              state = undefined,
                              ring_hash_new = "-1",
                              ring_hash_old = "-1",
                              when_is = 0,
                              error = 0}], Res1),

    ok = leo_manager_mnesia:update_storage_node_status(keep_state, NodeState0),
    {ok, Res2} = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual(true, length(Res2) == 1),

    ok = leo_manager_mnesia:update_storage_node_status(update_chksum, NodeState0#node_state{ring_hash_new = "12345",
                                                                                            ring_hash_old = "67890"}),
    {ok, Res3} = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual(true, length(Res3) == 1),

    ok = leo_manager_mnesia:update_storage_node_status(increment_error, NodeState0),
    {ok, Res4} = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual(true, length(Res4) == 1),

    ok = leo_manager_mnesia:update_storage_node_status(init_error, NodeState0),
    {ok, Res5} = leo_manager_mnesia:get_storage_nodes_all(),
    ?assertEqual(true, length(Res5) == 1),

    [NewNodeState0|_] = Res5,
    {error,  badarg} = leo_manager_mnesia:update_storage_node_status(badarg, NodeState0),

    %% gateway-node
    Node1  = 'test1@127.0.0.1',
    State1 = 'running',
    NodeState1 = #node_state{node = Node1,
                             state = State1},
    ok = leo_manager_mnesia:update_gateway_node(NodeState1),
    {ok, Res6} = leo_manager_mnesia:get_gateway_nodes_all(),
    ?assertEqual([#node_state{node  = Node1,
                              state = State1,
                              ring_hash_new = "-1",
                              ring_hash_old = "-1",
                              when_is = 0,
                              error = 0}], Res6),

    %% rebalance-info
    RebalanceInfo = #rebalance_info{vnode_id = 255,
                                    node = Node1,
                                    total_of_objects = 128,
                                    num_of_remains = 64,
                                    when_is = 0},
    ok = leo_manager_mnesia:update_rebalance_info(RebalanceInfo),
    {ok, [Res7|_]} = leo_manager_mnesia:get_rebalance_info_all(),
    ?assertEqual(RebalanceInfo, Res7),

    {ok, [Res8|_]} = leo_manager_mnesia:get_rebalance_info_by_node(Node1),
    ?assertEqual(RebalanceInfo, Res8),
    not_found = leo_manager_mnesia:get_rebalance_info_by_node(Node0),

    %%
    %% delete
    %%
    %% storage-node
    ok = leo_manager_mnesia:delete_storage_node(NewNodeState0),
    not_found = leo_manager_mnesia:get_storage_nodes_all(),

    %% backup/restore
    BakFile = "mnesia.bak",
    ok = leo_manager_mnesia:backup(BakFile),
    ok = leo_manager_mnesia:delete_all(),
    ok = leo_manager_mnesia:restore(BakFile),
    {ok, Res6} = leo_manager_mnesia:get_gateway_nodes_all(),
    {ok, [Res7|_]} = leo_manager_mnesia:get_rebalance_info_all(),
    not_found = leo_manager_mnesia:get_storage_nodes_all(),
    ok.

-endif.
