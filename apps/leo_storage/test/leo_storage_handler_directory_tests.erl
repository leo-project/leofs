%%====================================================================
%%
%% LeoStorage
%%
%% Copyright (c) 2012-2018 Rakuten, Inc.
%% Copyright (c) 2019-2025 Lions Data, Ltd.
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
-module(leo_storage_handler_directory_tests).

-include("leo_storage.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% TEST FUNCTIONS
%%--------------------------------------------------------------------
-ifdef(EUNIT).

object_handler_test_() ->
    {foreach, fun setup/0, fun teardown/1,
     [{with, [T]} || T <- [fun find_by_parent_dir_/1
                          ]]}.

setup() ->
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),

    Node0 = list_to_atom("node_0@" ++ Hostname),
    net_kernel:start([Node0, shortnames]),

    %% Use peer module instead of deprecated slave module
    {ok, Peer, Node1} = peer:start_link(#{name => node_1}),

    MeckPath = filename:dirname(code:which(meck)),
    rpc:call(Node0, code, add_path, [MeckPath]),
    rpc:call(Node1, code, add_path, [MeckPath]),

    [Node0, Node1, Peer].

teardown([_, _Node1, Peer]) ->
    catch meck:unload(),
    net_kernel:stop(),
    catch peer:stop(Peer),
    ok.


find_by_parent_dir_([Node0, _Node1, _Peer]) ->
    %% Use only local node to avoid RPC mock issues with peer nodes
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_members,
                fun() ->
                        Members = [#member{node  = Node0,
                                           state = ?STATE_RUNNING},
                                   #member{node  = 'stoped_node',
                                           state = ?STATE_STOP}
                                  ],
                        {ok, Members}
                end),

    %% Mock on local node (Node0 is the current test node)
    meck:new(leo_storage_handler_object, [non_strict]),
    meck:expect(leo_storage_handler_object, prefix_search,
                fun(_ParentDir, _,_) ->
                        {ok, [#?METADATA{key= <<"air/on/g/0.png">>},
                              #?METADATA{key= <<"air/on/g/1.png">>}]}
                end),

    {ok, Res} = leo_storage_handler_directory:find_by_parent_dir(<<"air/on/g/">>, none, none, 1000),
    ?assertEqual(2, length(Res)),

    ok.

-endif.
