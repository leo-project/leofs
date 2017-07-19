%%======================================================================
%%
%% LeoManager
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
%%======================================================================
-module(leo_manager_mnesia).

-include("leo_manager.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_libs.hrl").
-include_lib("stdlib/include/qlc.hrl").

%% API
-export([create_storage_nodes/2,
         create_gateway_nodes/2,
         create_rebalance_info/2,
         create_available_commands/2,
         create_del_bucket_state/2,

         get_storage_nodes_all/0,
         get_storage_node_by_name/1,
         get_storage_nodes_by_status/1,
         get_gateway_nodes_all/0,
         get_gateway_node_by_name/1,
         get_rebalance_info_all/0,
         get_rebalance_info_by_node/1,
         get_available_commands_all/0,
         get_available_command_by_name/1,
         get_del_bucket_state_all/0,
         get_del_bucket_state_by_bucket_name/1,
         get_del_bucket_state_by_bucket_name_and_node/2,
         get_del_bucket_state_by_state/1,

         update_storage_node_status/1,
         update_storage_node_status/2,
         update_gateway_node/1,
         update_rebalance_info/1,
         update_del_bucket_state/1,

         insert_available_command/2,
         insert_del_bucket_state/1,
         bulk_insert_del_bucket_info/1,

         delete_storage_node/1,
         delete_gateway_node/1,
         delete_del_bucket_state/1,
         bulk_delete_del_bucket_info/1,

         delete_all/0,
         backup/1,
         restore/1,

         update_available_commands/1
        ]).


%%-----------------------------------------------------------------------
%% Create Table
%%-----------------------------------------------------------------------
%% @doc Create storage-nodes table
-spec(create_storage_nodes(Mode, Nodes) ->
             ok | {error, any()} when Mode::ram_copies|disc_copies,
                                      Nodes::[node()]).
create_storage_nodes(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_STORAGE_NODES,
           [{Mode, Nodes},
            {type, set},
            {record_name, node_state},
            {attributes, record_info(fields, node_state)}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.


%% @doc Create gateway-nodes table
-spec(create_gateway_nodes(Mode, Nodes) ->
             ok | {error, any()} when Mode::ram_copies|disc_copies,
                                      Nodes::[node()]).
create_gateway_nodes(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_GATEWAY_NODES,
           [{Mode, Nodes},
            {type, set},
            {record_name, node_state},
            {attributes, record_info(fields, node_state)}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.


%% @doc Create rebalance-info table
-spec(create_rebalance_info(Mode, Nodes) ->
             ok | {error, any()} when Mode::ram_copies|disc_copies,
                                      Nodes::[node()]).
create_rebalance_info(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_REBALANCE_INFO,
           [{Mode, Nodes},
            {type, set},
            {record_name, rebalance_info},
            {attributes, record_info(fields, rebalance_info)}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.


%% @doc Create available commands table
-spec(create_available_commands(Mode, Nodes) ->
             ok | {error, any()} when Mode::ram_copies|disc_copies,
                                      Nodes::[node()]).
create_available_commands(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_AVAILABLE_CMDS,
           [{Mode, Nodes},
            {type, set},
            {record_name, cmd_state},
            {attributes, record_info(fields, cmd_state)}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.


%% @doc Create del-bucket-state table
-spec(create_del_bucket_state(Mode, Nodes) ->
             ok | {error, any()} when Mode::ram_copies|disc_copies,
                                      Nodes::[node()]).
create_del_bucket_state(Mode, Nodes) ->
    case mnesia:create_table(
           ?TBL_DEL_BUCKET_STATE,
           [{Mode, Nodes},
            {type, set},
            {record_name, del_bucket_state},
            {attributes, record_info(fields, del_bucket_state)}
           ]) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.


%%-----------------------------------------------------------------------
%% GET
%%-----------------------------------------------------------------------
%% @doc Retrieve all storage nodes
-spec(get_storage_nodes_all() ->
             {ok, [#node_state{}]} | not_found | {error, any()}).
get_storage_nodes_all() ->
    Tbl = ?TBL_STORAGE_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a storage node by node-name
-spec(get_storage_node_by_name(atom()) ->
             {ok, #node_state{}} | not_found | {error, any()}).
get_storage_node_by_name(Node) ->
    Tbl = ?TBL_STORAGE_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#node_state.node =:= Node]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve storage nodes by status
-spec(get_storage_nodes_by_status(atom()) ->
             {ok, [#node_state{}]} | not_found | {error, any()}).
get_storage_nodes_by_status(Status) ->
    Tbl = ?TBL_STORAGE_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#node_state.state =:= Status]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve all gateway nodes
-spec(get_gateway_nodes_all() ->
             {ok, [#node_state{}]} | not_found | {error, any()}).
get_gateway_nodes_all() ->
    Tbl = ?TBL_GATEWAY_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve gateway node info by node-name
-spec(get_gateway_node_by_name(atom()) ->
             {ok, #node_state{}} | not_found | {error, any()}).
get_gateway_node_by_name(Node) ->
    Tbl = ?TBL_GATEWAY_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#node_state.node =:= Node]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve rebalance info
-spec(get_rebalance_info_all() ->
             {ok, [#rebalance_info{}]} | not_found | {error, any()}).
get_rebalance_info_all() ->
    Tbl = ?TBL_REBALANCE_INFO,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(?TBL_REBALANCE_INFO)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve rebalance info by node
-spec(get_rebalance_info_by_node(atom()) ->
             {ok, [#rebalance_info{}]} | not_found | {error, any()}).
get_rebalance_info_by_node(Node) ->
    Tbl = ?TBL_REBALANCE_INFO,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#rebalance_info.node =:= Node]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve all available commands
-spec(get_available_commands_all() ->
             {ok, [#cmd_state{}]} | not_found | {error, any()}).
get_available_commands_all() ->
    Tbl = ?TBL_AVAILABLE_CMDS,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve available command by name
-spec(get_available_command_by_name(string()) ->
             {ok, [#cmd_state{}]} | not_found | {error, any()}).
get_available_command_by_name(Name) ->
    Tbl = ?TBL_AVAILABLE_CMDS,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#cmd_state.name =:= Name]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a list of del-bucket-state
-spec(get_del_bucket_state_all() ->
             {ok, [#del_bucket_state{}]} |
             not_found |
             {error, any()}).
get_del_bucket_state_all() ->
    Tbl = ?TBL_DEL_BUCKET_STATE,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, List} ->
                    ListBucketName = get_del_bucket_state_all_1(List, sets:new()),
                    get_del_bucket_state_all_2(ListBucketName, List, []);
                Other ->
                    Other
            end
    end.

%% @private
get_del_bucket_state_all_1([], Sets) ->
    lists:sort(sets:to_list(Sets));
get_del_bucket_state_all_1([#del_bucket_state{bucket_name = BucketName}|Acc], Sets) ->
    Sets_1 = sets:add_element(BucketName, Sets),
    get_del_bucket_state_all_1(Acc, Sets_1).

%% @private
get_del_bucket_state_all_2([],_StateL, Acc) ->
    {ok, lists:reverse(Acc)};
get_del_bucket_state_all_2([BucketName|ListBucketName], StateL, Acc) ->
    StateL_1 = lists:foldl(
                 fun(#del_bucket_state{bucket_name = B} = S, SAcc) when B == BucketName ->
                         [S|SAcc];
                    (_, SAcc) ->
                         SAcc
                 end, [], StateL),
    case StateL_1 of
        [] ->
            get_del_bucket_state_all_2(ListBucketName, StateL, Acc);
        _ ->
            get_del_bucket_state_all_2(ListBucketName, StateL, [{BucketName, StateL_1}|Acc])
    end.


%% @doc Retrieve a list of del-bucket-state by bucket-name
-spec(get_del_bucket_state_by_bucket_name(BucketName) ->
             {ok, [#del_bucket_state{}]} |
             not_found |
             {error, any()} when BucketName::binary()).
get_del_bucket_state_by_bucket_name(BucketName) ->
    Tbl = ?TBL_DEL_BUCKET_STATE,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#del_bucket_state.bucket_name =:= BucketName]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%% @doc Retrieve a list of del-bucket-state by bucket-name and node
-spec(get_del_bucket_state_by_bucket_name_and_node(BucketName, Node) ->
             {ok, [#del_bucket_state{}]} |
             not_found |
             {error, any()} when BucketName::binary(),
                                 Node::node()).
get_del_bucket_state_by_bucket_name_and_node(BucketName, Node) ->
    Tbl = ?TBL_DEL_BUCKET_STATE,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X ||
                                       X <- mnesia:table(Tbl),
                                       (X#del_bucket_state.bucket_name =:= BucketName andalso
                                        X#del_bucket_state.node =:= Node)]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            case leo_mnesia:read(F) of
                {ok, [H|_]} ->
                    {ok, H};
                Other ->
                    Other
            end
    end.


%% @doc Retrieve a list of del-bucket-state by state
-spec(get_del_bucket_state_by_state(State) ->
             {ok, [#del_bucket_state{}]} |
             not_found |
             {error, any()} when State::del_bucket_state()).
get_del_bucket_state_by_state(State) ->
    Tbl = ?TBL_DEL_BUCKET_STATE,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        Q1 = qlc:q([X || X <- mnesia:table(Tbl),
                                         X#del_bucket_state.state =:= State]),
                        Q2 = qlc:sort(Q1, [{order, ascending}]),
                        qlc:e(Q2)
                end,
            leo_mnesia:read(F)
    end.


%%-----------------------------------------------------------------------
%% UPDATE
%%-----------------------------------------------------------------------
%% @doc Modify a storage-node's status
-spec(update_storage_node_status(#node_state{}) ->
             ok | {error, any()}).
update_storage_node_status(NodeState) ->
    update_storage_node_status(update, NodeState).

-spec(update_storage_node_status(
        update|keep_state|update_chksum|increment_error|init_error, #node_state{}) ->
             ok | {error, any()}).
update_storage_node_status(update, NodeState) ->
    Tbl = ?TBL_STORAGE_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            NodeState_1 = NodeState#node_state{state = undefined},
            F = fun()-> mnesia:write(Tbl, NodeState_1, write) end,
            leo_mnesia:write(F)
    end;

update_storage_node_status(keep_state, NodeState) ->
    #node_state{node  = Node} = NodeState,
    case get_storage_node_by_name(Node) of
        {ok, Cur} ->
            update_storage_node_status(update, Cur#node_state{state = undefined});
        _ ->
            ok
    end;

update_storage_node_status(update_chksum, NodeState) ->
    #node_state{node = Node,
                ring_hash_new = RingHash0,
                ring_hash_old = RingHash1} = NodeState,

    case get_storage_node_by_name(Node) of
        {ok, Cur} ->
            update_storage_node_status(
              update, Cur#node_state{state = undefined,
                                     ring_hash_new = RingHash0,
                                     ring_hash_old = RingHash1});
        _ ->
            ok
    end;

update_storage_node_status(increment_error, NodeState) ->
    #node_state{node = Node} = NodeState,
    case get_storage_node_by_name(Node) of
        {ok, Cur} ->
            update_storage_node_status(
              update, Cur#node_state{state = undefined,
                                     error = Cur#node_state.error + 1});
        _ ->
            ok
    end;

update_storage_node_status(init_error, NodeState) ->
    #node_state{node = Node} = NodeState,
    case get_storage_node_by_name(Node) of
        {ok, Cur} ->
            update_storage_node_status(
              update, Cur#node_state{state = undefined,
                                     error = 0});
        _ ->
            ok
    end;
update_storage_node_status(_, _) ->
    {error, badarg}.


%% @doc Modify a gateway-node's status
-spec(update_gateway_node(#node_state{}) ->
             ok | {error, any()}).
update_gateway_node(NodeState) ->
    Tbl = ?TBL_GATEWAY_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() -> mnesia:write(Tbl, NodeState, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Modify a rebalance-info
-spec(update_rebalance_info(#rebalance_info{}) ->
             ok | {error, any()}).
update_rebalance_info(RebalanceInfo) ->
    Tbl = ?TBL_REBALANCE_INFO,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun()-> mnesia:write(Tbl, RebalanceInfo, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Modify a del-bucket-state
-spec(update_del_bucket_state(DelBucketState) ->
             ok | {error, any()} when DelBucketState::#del_bucket_state{}).
update_del_bucket_state(DelBucketState) ->
    Tbl = ?TBL_DEL_BUCKET_STATE,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun()-> mnesia:write(Tbl, DelBucketState, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Insert available commands
insert_available_command(Command, Help) ->
    Tbl = ?TBL_AVAILABLE_CMDS,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() -> mnesia:write(Tbl,
                                      #cmd_state{name = Command,
                                                 help = Help,
                                                 available = true}, write) end,
            leo_mnesia:write(F)
    end.


%% @doc Add a del-bucket-state
-spec(insert_del_bucket_state(DelBucketState) ->
             ok | {error, any()} when DelBucketState::#del_bucket_state{}).
insert_del_bucket_state(DelBucketState) ->
    update_del_bucket_state(DelBucketState).


%% @doc Add a list of del-bucket-state
-spec(bulk_insert_del_bucket_info(DelBucketStateList) ->
             ok | {error, any()} when DelBucketStateList::[#del_bucket_state{}]).
bulk_insert_del_bucket_info(DelBucketStateList) ->
    case mnesia:sync_transaction(
           fun() ->
                   bulk_insert_del_bucket_info_1(DelBucketStateList)
           end) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @private
bulk_insert_del_bucket_info_1([]) ->
    ok;
bulk_insert_del_bucket_info_1([DelBucketState|Acc]) ->
    case mnesia:write(?TBL_DEL_BUCKET_STATE, DelBucketState, write) of
        ok ->
            bulk_insert_del_bucket_info_1(Acc);
        _ ->
            {error, transaction_abort}
    end.


%%-----------------------------------------------------------------------
%% DELETE
%%-----------------------------------------------------------------------
%% @doc Remove storage-node by name
-spec(delete_storage_node(#node_state{}) ->
             ok | {error, any()}).
delete_storage_node(Node) when is_atom(Node) ->
    case get_storage_node_by_name(Node) of
        {ok, NodeInfo} ->
            delete_storage_node(NodeInfo);
        not_found ->
            ok;
        Error ->
            Error
    end;
delete_storage_node(Node) ->
    Tbl = ?TBL_STORAGE_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        mnesia:delete_object(Tbl, Node, write)
                end,
            leo_mnesia:delete(F)
    end.


%% @doc Remove gateway-node by name
-spec(delete_gateway_node(atom() | #node_state{}) ->
             ok | {error, any()}).
delete_gateway_node(Node) when is_atom(Node) ->
    case get_gateway_node_by_name(Node) of
        {ok, NodeInfo} ->
            delete_gateway_node(NodeInfo);
        not_found ->
            ok;
        Error ->
            Error
    end;
delete_gateway_node(NodeInfo) ->
    Tbl = ?TBL_GATEWAY_NODES,

    case catch mnesia:table_info(Tbl, all) of
        {'EXIT', _Cause} ->
            {error, ?ERROR_MNESIA_NOT_START};
        _ ->
            F = fun() ->
                        mnesia:delete_object(Tbl, NodeInfo, write)
                end,
            leo_mnesia:delete(F)
    end.


%% @doc Remove a del-bucket-state
-spec(delete_del_bucket_state(BucketName) ->
             ok | {error, any()} when BucketName::binary()).
delete_del_bucket_state(BucketName) ->
    case get_del_bucket_state_by_bucket_name(BucketName) of
        {ok, DelBucketState} ->
            bulk_delete_del_bucket_info(DelBucketState);
        not_found ->
            ok;
        Error ->
            Error
    end.


%% @doc Remove a list of del-bucket-state
-spec(bulk_delete_del_bucket_info(DelBucketStateList) ->
             ok | {error, any()} when DelBucketStateList::[#del_bucket_state{}]).
bulk_delete_del_bucket_info(DelBucketStateList) ->
    case mnesia:sync_transaction(
           fun() ->
                   bulk_delete_del_bucket_info_1(DelBucketStateList)
           end) of
        {atomic, ok} ->
            ok;
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @private
bulk_delete_del_bucket_info_1([]) ->
    ok;
bulk_delete_del_bucket_info_1([DelBucketState|Acc]) ->
    case mnesia:delete_object(?TBL_DEL_BUCKET_STATE, DelBucketState, write) of
        ok ->
            bulk_delete_del_bucket_info_1(Acc);
        _ ->
            {error, transaction_abort}
    end.


%% @doc Delete all tables
-spec(delete_all() ->
             ok | {error, any()}).
delete_all() ->
    {atomic, ok} = mnesia:delete_table(?TBL_SYSTEM_CONF),
    {atomic, ok} = mnesia:delete_table(?TBL_GATEWAY_NODES),
    {atomic, ok} = mnesia:delete_table(?TBL_STORAGE_NODES),
    {atomic, ok} = mnesia:delete_table(?TBL_AVAILABLE_CMDS),
    {atomic, ok} = mnesia:delete_table(?TBL_REBALANCE_INFO),
    {atomic, ok} = mnesia:delete_table(?TBL_DEL_BUCKET_STATE),
    ok.

%% @doc Backup mnesia tables
-spec(backup(file:filename()) ->
             ok | {error, any()}).
backup(DstFilePath) ->
    {ok, Name, _Nodes} = mnesia:activate_checkpoint(
                           [{ram_overrides_dump, true},
                            {name, "backup"},
                            {max,[?TBL_SYSTEM_CONF,
                                  ?TBL_GATEWAY_NODES,
                                  ?TBL_STORAGE_NODES,
                                  ?TBL_AVAILABLE_CMDS,
                                  ?TBL_REBALANCE_INFO,
                                  ?TBL_DEL_BUCKET_STATE,
                                  ?ENDPOINT_TABLE,
                                  ?AUTH_TABLE,
                                  ?BUCKET_TABLE,
                                  ?USERS_TABLE,
                                  ?USER_CREDENTIAL_TABLE]}]),
    try
        mnesia:backup_checkpoint(Name, DstFilePath)
    after
        mnesia:deactivate_checkpoint(Name)
    end.

%% @doc Restore mnesia tables
-spec(restore(file:filename()) ->
             ok | {error, any()}).
restore(DstFilePath) ->
    case mnesia:restore(DstFilePath, [{default_op, recreate_tables}]) of
        {atomic, RestoredTabs} ->
            validate_restored_tables(RestoredTabs);
        {aborted, Reason} ->
            {error, Reason}
    end.

%% @private validate restored tables
validate_restored_tables(RestoredTabs) ->
    TableSet = sets:new(),
    TableSet2 = sets:add_element(?TBL_SYSTEM_CONF, TableSet),
    TableSet3 = sets:add_element(?TBL_GATEWAY_NODES, TableSet2),
    TableSet4 = sets:add_element(?TBL_STORAGE_NODES, TableSet3),
    TableSet5 = sets:add_element(?TBL_AVAILABLE_CMDS, TableSet4),
    TableSet6 = sets:add_element(?TBL_REBALANCE_INFO, TableSet5),
    TableSet7 = sets:add_element(?TBL_DEL_BUCKET_STATE, TableSet6),
    TableSet8 = sets:add_element(?ENDPOINT_TABLE, TableSet7),
    TableSet9 = sets:add_element(?AUTH_TABLE, TableSet8),
    TableSet10 = sets:add_element(?BUCKET_TABLE, TableSet9),
    TableSet11 = sets:add_element(?USERS_TABLE, TableSet10),
    TableSet12 = sets:add_element(?USER_CREDENTIAL_TABLE, TableSet11),
    validate_restored_tables(RestoredTabs, TableSet12).

validate_restored_tables([], ExpectedTableSet) ->
    case sets:size(ExpectedTableSet) of
        0 ->
            ok;
        _ ->
            {error, {missing_tables, ExpectedTableSet}}
    end;
validate_restored_tables([T|Rest], ExpectedTableSet) ->
    validate_restored_tables(Rest, sets:del_element(T, ExpectedTableSet)).


%% @doc Update available commands
-spec(update_available_commands(atom | list()) ->
             ok).
update_available_commands(AvailableCommands) ->
    {atomic,ok} = mnesia:clear_table(?TBL_AVAILABLE_CMDS),
    case AvailableCommands of
        all ->
            lists:foreach(
              fun({C, H}) ->
                      leo_manager_mnesia:insert_available_command(C,H)
              end, ?COMMANDS);
        CmdL ->
            lists:foreach(
              fun({C1, H}) ->
                      case lists:foldl(
                             fun(C2, false) when C1 == C2 -> true;
                                (_,  Ret) -> Ret
                             end, false, CmdL) of
                          true ->
                              leo_manager_mnesia:insert_available_command(C1,H);
                          false ->
                              void
                      end
              end, ?COMMANDS)
    end,
    ok.
