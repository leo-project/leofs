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
-module(leo_manager_ring_sync).

-behaviour(gen_server).

-include("leo_manager.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(sync_state, {timeout = ?DEF_RING_SYNC_INTERVAL :: non_neg_integer()}).
-define(SVR_STORAGE,   'storage').
-define(SVR_GATEWAY,   'gateway').
-define(SVR_MGR_SLAVE, 'manager_slave').


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link(?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    Timeout = ?DEF_RING_SYNC_INTERVAL,
    {ok, #sync_state{timeout = Timeout}, Timeout}.

handle_call(_Request, _From, #sync_state{timeout = Timeout} = State) ->
    Reply = ok,
    {reply, Reply, State, Timeout}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, #sync_state{timeout = Timeout} = State) ->
    {noreply, State, Timeout}.

handle_info(timeout, State=#sync_state{timeout = Timeout}) ->
    ok = sync_ring(leo_manager_mnesia:get_storage_nodes_all(), ?SVR_STORAGE),
    ok = sync_ring(leo_manager_mnesia:get_gateway_nodes_all(), ?SVR_GATEWAY),
    case ?env_mode_of_manager() of
        'master' ->
            case ?env_partner_of_manager_node() of
                [] ->
                    ok;
                [SlaveNode|_] ->
                    sync_ring(SlaveNode)
            end;
        _ ->
            ok
    end,
    {noreply, State, Timeout}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%-----------------------------------------------------------------------
%% Internal Functions
%%-----------------------------------------------------------------------
%% @doc Synchronize the ring with the manager-slave node
%% @private
sync_ring(SlaveNode) ->
    RingChecksums = get_local_checksum(),
    sync_ring_1([{SlaveNode,-1,-1}], RingChecksums, ?SVR_MGR_SLAVE).

%% @doc Synchronize the ring with gateway/storage nodes
%% @private
sync_ring({ok, Nodes}, ServerType) ->
    Nodes_1 = [{N, C1, C2} || #node_state{node = N,
                                          ring_hash_new = C1,
                                          ring_hash_old = C2} <- Nodes],
    RingChecksums = get_local_checksum(),
    sync_ring_1(Nodes_1, RingChecksums, ServerType);
sync_ring(_,_) ->
    ok.


%% @doc Retrieve the local ring's checksum
%% @private
get_local_checksum() ->
    {ok, {OrgC1, OrgC2}} = leo_redundant_manager_api:checksum(?CHECKSUM_RING),
    OrgC1_1 = case (OrgC1 > 0) of
                  true  -> leo_hex:integer_to_hex(OrgC1, 8);
                  false -> OrgC1
              end,
    OrgC2_1 = case (OrgC2 > 0) of
                  true  -> leo_hex:integer_to_hex(OrgC2, 8);
                  false -> OrgC2
              end,
    {OrgC1_1, OrgC2_1}.


%% @private
sync_ring_1([],_,_) ->
    ok;
sync_ring_1([{_, C1, C2}|Rest], {C1, C2} = Chksum, ServerType) ->
    sync_ring_1(Rest, Chksum, ServerType);
sync_ring_1([{Node,_C1,_C2}|Rest], {OrgC1, OrgC2} = Chksum, ServerType) ->
    case rpc:call(Node, leo_redundant_manager_api, checksum, [ring]) of
        {ok, {C1_1, C2_1}} ->
            C1_2 = leo_hex:integer_to_hex(C1_1, 8),
            C2_2 = leo_hex:integer_to_hex(C2_1, 8),

            case (C1_2 == OrgC1 andalso
                  C2_2 == OrgC2) of
                true when ServerType == ?SVR_STORAGE ->
                    case leo_manager_mnesia:get_storage_node_by_name(Node) of
                        {ok, NodeState} ->
                            leo_manager_mnesia:update_storage_node_status(
                              update_chksum,
                              NodeState#node_state{node  = Node,
                                                   ring_hash_new = C1_2,
                                                   ring_hash_old = C2_2});
                        _ ->
                            void
                    end;
                true when ServerType == ?SVR_GATEWAY ->
                    case leo_manager_mnesia:get_gateway_node_by_name(Node) of
                        {ok, NodeState} ->
                            leo_manager_mnesia:update_gateway_node(
                              NodeState#node_state{ring_hash_new = C1_2,
                                                   ring_hash_old = C2_2});
                        _ ->
                            void
                    end;
                true ->
                    ok;
                false ->
                    catch leo_manager_api:recover(?RECOVER_RING, Node, true)
            end;
        _ ->
            void
    end,
    sync_ring_1(Rest, Chksum, ServerType).
