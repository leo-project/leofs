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
%%======================================================================
-module(leo_storage_handler_sync).

-behaviour(leo_mdcr_sync_cluster_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([get_metadatas/1,
         force_sync/1,
         send_addrid_and_key_to_remote/2
        ]).
-export([handle_call/1]).

-define(DEF_THRESHOLD_LEN, 1000).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Compare local-metadatas with remote-metadatas
-spec(get_metadatas(ListOfAddrAndKey) ->
             {ok, [#?METADATA{}]} when ListOfAddrAndKey::[tuple()]).
get_metadatas(ListOfAddrAndKey) ->
    get_metadatas_1(ListOfAddrAndKey, []).

%% @private
get_metadatas_1([], Acc) ->
    {ok, Acc};
get_metadatas_1([AddrAndKey|Rest], Acc) ->
    case catch leo_object_storage_api:head(AddrAndKey) of
        {ok, MetaBin} ->
            Metadata = binary_to_term(MetaBin),
            get_metadatas_1(Rest, [Metadata|Acc]);
        _Other ->
            {AddrId, Key} = AddrAndKey,
            get_metadatas_1(Rest, [#?METADATA{addr_id = AddrId,
                                              key = Key}|Acc])
    end.


%% @doc Synchronize object with remote-cluster(s)
%%
-spec(force_sync(ClusterId) ->
             ok when ClusterId::atom()).
force_sync(ClusterId) when is_atom(ClusterId) ->
    case leo_mdcr_tbl_cluster_stat:get(ClusterId) of
        {ok, #?CLUSTER_STAT{state = ?STATE_RUNNING}} ->
            handle_call(ClusterId);
        {ok,_} ->
            {error, "Not running"};
        not_found ->
            {error, "Not found"};
        Error ->
            Error
    end;
force_sync(_) ->
    {error, invalid_parameter}.


%% @doc Send list of metadatas to remote-storage(s),
%%      then compare with metadatas of remote-storage
%%      if found an inconsist metadata then fix it
%%
-spec(send_addrid_and_key_to_remote(ClusterId, ListOfAddrIdAndKey) ->
             ok when ClusterId::atom(),
                     ListOfAddrIdAndKey::[tuple()]).
send_addrid_and_key_to_remote(ClusterId, ListAddrIdAndKey) ->
    case leo_sync_remote_cluster:get_cluster_members(ClusterId) of
        {ok, ListMembers} ->
            send_addrid_and_key_to_remote_1(
              ListMembers, ClusterId, ListAddrIdAndKey);
        {error, Cause} ->
            ?warn("send_addrid_and_key_to_remote/1", [{cause, Cause}]),
            ok
    end.

%% @private
send_addrid_and_key_to_remote_1([],_ClusterId,_ListAddrIdAndKey) ->
    ok;
send_addrid_and_key_to_remote_1([#mdc_replication_info{
                                    cluster_members = Members}|Rest],
                                ClusterId, ListAddrIdAndKey) ->
    case send_addrid_and_key_to_remote_2(
           Members, ClusterId, ListAddrIdAndKey, 0) of
        {ok, RetL} ->
            ok = leo_sync_remote_cluster:compare_metadata(RetL),
            send_addrid_and_key_to_remote_1(Rest, ClusterId, ListAddrIdAndKey);
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
send_addrid_and_key_to_remote_2([], ClusterId, ListAddrIdAndKey,_RetryTimes) ->
    QId = ?QUEUE_ID_COMP_META_WITH_DC,
    case leo_storage_mq:publish(
           QId, ClusterId, ListAddrIdAndKey) of
        ok ->
            {ok, []};
        {error, Cause} ->
            ?warn("send_addrid_and_key_to_remote/1",
                  [{qid, QId}, {cluster_id, ClusterId},
                   {list_addr_id_and_key,  ListAddrIdAndKey},
                   {cause, Cause}]),
            {error, Cause}
    end;
send_addrid_and_key_to_remote_2([_|Rest], ClusterId, ListAddrIdAndKey, ?DEF_MAX_RETRY_TIMES) ->
    send_addrid_and_key_to_remote_2(Rest, ClusterId, ListAddrIdAndKey, 0);
send_addrid_and_key_to_remote_2([#?CLUSTER_MEMBER{
                                     node = Node,
                                     port = Port}|Rest] = ClusterMembes,
                                ClusterId, ListAddrIdAndKey, RetryTimes) ->
    Node_1 = list_to_atom(lists:append([atom_to_list(Node),
                                        ":",
                                        integer_to_list(Port)])),
    case leo_rpc:call(Node_1, erlang, node, []) of
        Msg when Msg == timeout orelse
                 element(1, Msg) == badrpc ->
            send_addrid_and_key_to_remote_2(
              ClusterMembes, ClusterId, ListAddrIdAndKey, RetryTimes + 1);
        _ ->
            Timeout = ?env_mdcr_req_timeout(),
            Ret = case leo_rpc:call(Node_1, ?MODULE, get_metadatas,
                                    [ListAddrIdAndKey], Timeout) of
                      {ok, RetL} ->
                          {ok, RetL};
                      {error, Cause} ->
                          {error, Cause};
                      {badrpc, Cause} ->
                          {error, Cause};
                      timeout = Cause ->
                          {error, Cause}
                  end,

            case Ret of
                {ok, _} ->
                    Ret;
                {error,_Cause} ->
                    send_addrid_and_key_to_remote_2(
                      Rest, ClusterId, ListAddrIdAndKey, 0)
            end
    end.


%%--------------------------------------------------------------------
%% CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Handle sync objects with a remote storage cluster
%%
handle_call(ClusterId) ->
    send_metadata(ClusterId).


%%--------------------------------------------------------------------
%% INNTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc synchronize objects with a remote-cluster
%% @private
-spec(send_metadata(ClusterId) ->
             ok when ClusterId::atom()).
send_metadata(ClusterId) ->
    Callback = send_addrid_and_key_callback(ClusterId),
    case catch leo_object_storage_api:fetch_by_addr_id(0, Callback) of
        {ok, []} ->
            ok;
        {ok, RetL} ->
            send_addrid_and_key_to_remote(ClusterId, RetL);
        {'EXIT', Cause} ->
            {error, Cause};
        Error ->
            Error
    end.

%% @private
%% -spec(send_addrid_and_key_callback(atom()) ->
%%              ok).
send_addrid_and_key_callback(ClusterId) ->
    fun(K, V, Acc) when length(Acc) >= ?DEF_THRESHOLD_LEN ->
            ok = send_addrid_and_key_to_remote(ClusterId, Acc),
            timer:sleep(timer:seconds(1)),
            send_addrid_and_key_callback_1(K, V, []);
       (K, V, Acc) ->
            send_addrid_and_key_callback_1(K, V, Acc)
    end.

%% @private
send_addrid_and_key_callback_1(_K, V, Acc) ->
    Metadata_1 = binary_to_term(V),
    Metadata_2 = leo_object_storage_transformer:transform_metadata(Metadata_1),
    #?METADATA{addr_id = AddrId} = Metadata_2,

    case leo_redundant_manager_api:get_redundancies_by_addr_id(put, AddrId) of
        {ok, #redundancies{nodes = Redundancies}} ->
            Nodes = [N || #redundant_node{node = N,
                                          available = true} <- Redundancies],
            send_addrid_and_key_callback_2(Nodes, Metadata_2, Acc);
        _Other ->
            Acc
    end.

%% @doc Retrieve an object,
%%      then it is stacked into a buffer of a transfer
%% @private
send_addrid_and_key_callback_2([Node|_],
                               #?METADATA{key = Key,
                                          addr_id = AddrId}, Acc) when Node == erlang:node() ->
    [{AddrId, Key}|Acc];
send_addrid_and_key_callback_2(_,_,Acc) ->
    Acc.
