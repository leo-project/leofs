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
%%======================================================================
-module(leo_sync_remote_cluster).
-behaviour(leo_ordning_reda_behaviour).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_ordning_reda/include/leo_ordning_reda.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/0, start_link/1, stop/1,
         defer_stack/1, stack/1, stack/2, store/2,
         gen_id/0, gen_id/1,
         get_cluster_members/1, compare_metadata/1]).
-export([handle_send/3,
         handle_fail/2]).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Add a container into the supervisor
-spec(start_link() ->
             ok | {error, any()}).
start_link() ->
    start_link(gen_id()).

-spec(start_link(UId) ->
             ok | {error, any()} when UId::atom()).
start_link(UId) ->
    BufSize = ?env_mdcr_sync_proc_buf_size(),
    Timeout = ?env_mdcr_sync_proc_timeout(),
    leo_ordning_reda_api:add_container(UId, [{module,      ?MODULE},
                                             {buffer_size, BufSize},
                                             {timeout,     Timeout}]).


%% @doc Remove a container from the supervisor
-spec(stop(Id) ->
             ok | {error, any()} when Id::atom()).
stop(Id) ->
    leo_ordning_reda_api:remove_container(Id).


%% @doc Stack a object into the ordning&reda
-spec(defer_stack(Object) ->
             ok | {error, any()} when Object::null|#?OBJECT{}).
defer_stack(#?OBJECT{} = Object) ->
    case (leo_mdcr_tbl_cluster_stat:size() > 0) of
        true ->
            Ret = leo_mdcr_tbl_cluster_stat:find_by_state(?STATE_RUNNING),
            defer_stack_1(Ret, Object);
        false ->
            ok
    end;
defer_stack(_) ->
    ok.

%% @private
defer_stack_1({ok,_}, #?OBJECT{addr_id = AddrId,
                               key = Key} = Object) ->
    case stack(Object) of
        ok ->
            ok;
        {error, Cause}->
            ?warn("defer_stack_1/1",
                  [{addr_id, AddrId},
                   {key, Key}, {cause, Cause}]),

            QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
            case leo_storage_mq:publish(
                   QId, AddrId, Key) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn("defer_stack/1",
                          [{addr_id, AddrId},
                           {key, Key}, {cause, Cause}]),
                    {error, Cause}
            end
    end;
defer_stack_1(not_found,_) ->
    ok;
defer_stack_1({error, Cause}, #?OBJECT{addr_id = AddrId,
                                       key = Key}) ->
    ?warn("defer_stack_1/1",
          [{addr_id, AddrId},
           {key, Key}, {cause, Cause}]),
    {error, Cause}.


%% @doc Stack a object into the ordning&reda
-spec(stack(Object) ->
             ok | {error, any()} when Object::#?OBJECT{}).
stack(Object) ->
    stack(undefined, Object).

-spec(stack(ClusterId, Object) ->
             ok | {error, any()} when ClusterId::atom(),
                                      Object::#?OBJECT{}).
stack(ClusterId, Object) ->
    stack_fun(ClusterId, Object).


%% @doc Store stacked objects
-spec(store(ClusterId, BinObjs) ->
             {ok, [#?METADATA{}]} | {error, any()} when ClusterId::atom(),
                                                        BinObjs::binary()).
store(ClusterId, BinObjs) ->
    case slice_and_replicate(ClusterId, BinObjs, []) of
        {ok, RetL} ->
            {ok, RetL};
        {error, _Cause} ->
            {error, fail_storing_files}
    end.


%% @doc Generate a sync-proc of ID
-spec(gen_id() ->
             atom()).
gen_id() ->
    gen_id(erlang:phash2(leo_date:clock(),
                         ?env_num_of_mdcr_sync_procs()) + 1).

-spec(gen_id(Id) ->
             atom() when Id::pos_integer() |
                             {'cluster_id', (atom()|string()|pos_integer())}).
gen_id(Id) when is_integer(Id) ->
    list_to_atom(lists:append([?DEF_PREFIX_MDCR_SYNC_PROC_1, integer_to_list(Id)]));
gen_id({cluster_id, ClusterId}) ->
    ClusterId_1 = case is_atom(ClusterId) of
                      true  -> atom_to_list(ClusterId);
                      false -> ClusterId
                  end,
    list_to_atom(lists:append([?DEF_PREFIX_MDCR_SYNC_PROC_2, ClusterId_1])).


%% @doc Retrieve cluster members
-spec(get_cluster_members(ClusterId) ->
             {ok, [#mdc_replication_info{}]} |
             {error, any()} when ClusterId::atom()).
get_cluster_members(undefined) ->
    case leo_mdcr_tbl_cluster_info:all() of
        {ok, ClusterInfoList} ->
            get_cluster_members_1(ClusterInfoList);
        {error, Cause} ->
            {error, Cause}
    end;
get_cluster_members(ClusterId) ->
    get_cluster_members_1([#?CLUSTER_INFO{cluster_id = ClusterId}]).

%% @private
get_cluster_members_1(ClusterInfoList) ->
    case leo_cluster_tbl_conf:get() of
        {ok, #?SYSTEM_CONF{num_of_dc_replicas = NumOfReplicas}} ->
            case get_cluster_members_2(ClusterInfoList, NumOfReplicas, []) of
                {ok, MDCR_Info} ->
                    {ok,  MDCR_Info};
                {error, Cause} ->
                    {error, Cause}
            end;
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
get_cluster_members_2([],_NumOfReplicas, Acc) ->
    {ok, Acc};
get_cluster_members_2([#?CLUSTER_INFO{cluster_id = ClusterId}|Rest],
                      NumOfReplicas, Acc) ->
    case leo_cluster_tbl_conf:get() of
        {ok, #?SYSTEM_CONF{cluster_id = ClusterId}} ->
            get_cluster_members_2(Rest, NumOfReplicas, Acc);
        _ ->
            case leo_mdcr_tbl_cluster_member:find_by_limit_with_rnd(
                   ClusterId, ?DEF_NUM_OF_REMOTE_MEMBERS) of
                {ok, ClusterMembers} ->
                    get_cluster_members_2(Rest, NumOfReplicas,
                                          [#mdc_replication_info{cluster_id = ClusterId,
                                                                 num_of_replicas = NumOfReplicas,
                                                                 cluster_members = ClusterMembers}
                                           |Acc]);
                not_found = Cause ->
                    {error, Cause};
                {error, Cause} ->
                    {error, Cause}
            end
    end.


%% @doc Compare a local-metadata with a remote-metadata
%%      If it's inconsistent, the metadata is put into the queue
-spec(compare_metadata(ListOfMetadata) ->
             ok when ListOfMetadata::[#?METADATA{}]).
compare_metadata([]) ->
    ok;
compare_metadata([#?METADATA{addr_id = AddrId,
                             key = Key} = Metadata|Rest]) ->
    Ret = leo_object_storage_api:head({AddrId, Key}),
    ok = compare_metadata_1(Ret, Metadata),
    compare_metadata(Rest).

%% @private
compare_metadata_1({ok, MetaBin}, #?METADATA{cluster_id = ClusterId,
                                             addr_id = AddrId,
                                             key = Key,
                                             clock = Clock,
                                             del = Del}) ->
    #?METADATA{clock = Clock_1,
               del = Del_1} = binary_to_term(MetaBin),
    case (Clock == Clock_1 andalso
          Del == Del_1) of
        true ->
            ok;
        false ->
            QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
            case leo_storage_mq:publish(
                   QId, ClusterId, AddrId, Key) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?warn("compare_metadata_1/2",
                          [{qid, QId}, {cluster_id, ClusterId},
                           {addr_id, AddrId}, {key, Key}, {cause, Cause}]),
                    ok
            end
    end;
compare_metadata_1(not_found, #?METADATA{del = ?DEL_TRUE}) ->
    ok;
compare_metadata_1(not_found, #?METADATA{cluster_id = ClusterId,
                                         addr_id = AddrId,
                                         key = Key,
                                         del = ?DEL_FALSE}) ->
    QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
    case leo_storage_mq:publish(QId, ClusterId, AddrId, Key, ?DEL_TRUE) of
        ok ->
            ok;
        {error, Cause} ->
            ?warn("compare_metadata_1/2",
                  [{qid, QId}, {cluster_id, ClusterId},
                   {addr_id, AddrId}, {key, Key}, {cause, Cause}]),
            ok
    end;
compare_metadata_1({_,Cause}, #?METADATA{cluster_id = ClusterId,
                                         addr_id = AddrId,
                                         key = Key}) ->
    ?warn("comapare_metadata_1/2",
          [{key, binary_to_list(Key)},
           {cause, Cause}]),

    QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
    case leo_storage_mq:publish(QId, ClusterId, AddrId, Key) of
        ok ->
            ok;
        {error, Cause} ->
            ?warn("compare_metadata_1/2",
                  [{qid, QId}, {cluster_id, ClusterId},
                   {addr_id, AddrId}, {key, Key}, {cause, Cause}]),
            ok
    end.

%%--------------------------------------------------------------------
%% Callback
%%--------------------------------------------------------------------
%% @doc Handle send object to a remote-node.
handle_send(UId, StackedInfo, BinObjs) ->
    %% Retrieve cluser-id
    ClusterId = get_cluster_id_from_uid(UId),

    %% Retrieve remote-members from the tables
    case get_cluster_members(ClusterId) of
        {ok, ListMembers} ->
            send(ListMembers, StackedInfo, BinObjs);
        {error,_Reason} ->
            {error,_Reason}
    end.


%% @doc Handle a fail process
-spec(handle_fail(atom(), list({integer(), string()})) ->
             ok | {error, any()}).
handle_fail(_, []) ->
    ok;
handle_fail(UId, [{AddrId, Key}|Rest] = _StackInfo) ->
    case get_cluster_id_from_uid(UId) of
        undefined ->
            QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
            case leo_storage_mq:publish(QId, AddrId, Key) of
                ok ->
                    void;
                {error, Cause} ->
                    ?warn("handle_fail/2",
                          [{qid, QId}, {addr_id, AddrId},
                           {key, Key}, {cause, Cause}])
            end;
        ClusterId ->
            QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
            case leo_storage_mq:publish(QId, ClusterId, AddrId, Key) of
                ok ->
                    void;
                {error, Cause} ->
                    ?warn("handle_fail/2",
                          [{qid, QId}, {cluster_id, ClusterId},
                           {addr_id, AddrId}, {key, Key}, {cause, Cause}])
            end
    end,
    handle_fail(UId, Rest).


%%--------------------------------------------------------------------
%% INNER FUNCTION
%%--------------------------------------------------------------------
%% @doc Stack an object into "ordning-reda"
%% @private
-spec(stack_fun(atom(), #?OBJECT{}) ->
             ok | {error, any()}).
stack_fun(ClusterId, #?OBJECT{addr_id = AddrId,
                              key = Key,
                              meta = CMeta} = Object) ->
    case leo_cluster_tbl_conf:get() of
        {ok, #?SYSTEM_CONF{
                 cluster_id = MDC_ClusterId,
                 num_of_dc_replicas = MDCR_N,
                 mdcr_r = MDCR_R,
                 mdcr_w = MDCR_W,
                 mdcr_d = MDCR_D}} ->
            UDM_1 = case leo_object_storage_transformer:get_udm_from_cmeta_bin(CMeta) of
                        {ok, UDM} when UDM /= [] ->
                            UDM;
                        _ ->
                            []
                    end,
            CMeta_1 = leo_object_storage_transformer:list_to_cmeta_bin(
                         [{?PROP_CMETA_CLUSTER_ID, MDC_ClusterId},
                          {?PROP_CMETA_NUM_OF_REPLICAS, MDCR_N},
                          {?PROP_CMETA_UDM, UDM_1}
                         ]),
            CMetaLen = byte_size(CMeta_1),
            Object_1 =  Object#?OBJECT{cluster_id = MDC_ClusterId,
                                       num_of_replicas = MDCR_N,
                                       preferred_r = MDCR_R,
                                       preferred_w = MDCR_W,
                                       preferred_d = MDCR_D,
                                       msize = CMetaLen,
                                       meta = CMeta_1},
            ObjBin = term_to_binary(Object_1),
            ObjSize = byte_size(ObjBin),
            Data = << ObjSize:?DEF_BIN_OBJ_SIZE, ObjBin/binary,
                      ?DEF_BIN_PADDING/binary >>,
            UId = case ClusterId of
                      undefined ->
                          gen_id();
                      _ ->
                          gen_id({cluster_id, ClusterId})
                  end,
            stack_fun(UId, AddrId, Key, Data);
        _Other ->
            {error, ?ERROR_COULD_NOT_GET_SYSTEM_CONF}
    end.

stack_fun(UId, AddrId, Key, Data) ->
    case leo_ordning_reda_api:stack(UId, {AddrId, Key}, Data) of
        ok ->
            ok;
        {error, undefined} ->
            case start_link(UId) of
                ok ->
                    stack_fun(UId, AddrId, Key, Data);
                {error, Cause} ->
                    {error, Cause}
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Slicing objects and Store objects
%% @private
-spec(slice_and_replicate(atom(), binary(), [#?METADATA{}]) ->
             {ok, [#?METADATA{}]} | {error, any()}).
slice_and_replicate(_, <<>>, Acc) ->
    {ok, lists:flatten(Acc)};
slice_and_replicate(ClusterId, StackedObjs, Acc) ->
    %% Retrieve a metadata and an object from stacked objects,
    %% then replicate an object into the storage cluster
    case slice(StackedObjs) of
        {ok, Object, StackedObjs_1} ->
            Ret = replicate(ClusterId, Object),
            slice_and_replicate(ClusterId, StackedObjs_1, [Ret|Acc]);
        {error, Cause}->
            {error, Cause}
    end.

%% @private
slice(StackedObjs) ->
    try
        %% Retrieve object
        << ObjSize:?DEF_BIN_OBJ_SIZE, Rest_1/binary >> = StackedObjs,
        ObjBin = binary:part(Rest_1, {0, ObjSize}),

        Object = binary_to_term(ObjBin),
        Rest_2 = binary:part(Rest_1, {ObjSize, byte_size(Rest_1) - ObjSize}),

        %% Retrieve footer
        <<_Fotter:64, Rest_3/binary>> = Rest_2,
        {ok, Object, Rest_3}
    catch
        _:Cause ->
            ?error("slice/1", [{cause, Cause}]),
            {error, invalid_format}
    end.


%% @doc Replicate an object between clusters
%% @private
-spec(replicate(atom(), #object{} | #object_1{} | #?OBJECT{}) ->
             [] | #?METADATA{}).
replicate(ClusterId, #?OBJECT{num_of_replicas = MDCR_N} = Object) ->
    %% Retrieve redundancies of the cluster,
    %%   then overwrite 'n' and 'w' for the mdc-replication
    Ret = case (MDCR_N == 0) of
              true ->
                  case leo_mdcr_tbl_cluster_info:get(ClusterId) of
                      {ok, #?CLUSTER_INFO{num_of_dc_replicas = MDCR_1}} ->
                          {ok, MDCR_1};
                      {error, Cause} ->
                          ?warn("replicate/2", [{cause, Cause}]),
                          {error, Cause}
                  end;
              false ->
                  {ok, MDCR_N}
          end,
    replicate_1(Ret, Object#?OBJECT{cluster_id = ClusterId});
replicate(ClusterId, Object) ->
    replicate(ClusterId, leo_object_storage_transformer:transform_object(Object)).

%% @private
replicate_1({ok, MDCR_N}, Object) ->
    Object_1 = Object#?OBJECT{num_of_replicas = MDCR_N},
    Ret = case leo_storage_handler_object:replicate(Object_1) of
              {ok,_ETag} ->
                  {ok, leo_object_storage_transformer:object_to_metadata(Object_1)};
              {error, Cause} ->
                  ?warn("replicate_1/1", [{cause, Cause}]),
                  {error, Cause}
          end,
    replicate_2(Ret);
replicate_1(Ret,_Object) ->
    replicate_2(Ret).

%% @private
replicate_2({ok, Metadata}) ->
    Metadata;
replicate_2(_) ->
    [].


%% @doc Retrieve cluster-id from uid
%% @private
get_cluster_id_from_uid(UId) when is_atom(UId) ->
    UIdStr = atom_to_list(UId),
    case string:str(UIdStr,
                    ?DEF_PREFIX_MDCR_SYNC_PROC_2) of
        0 ->
            undefined;
        _ ->
            string:sub_string(
              UIdStr, length(?DEF_PREFIX_MDCR_SYNC_PROC_2))
    end;
get_cluster_id_from_uid(_) ->
    undefined.


%% Send stacked objects to remote-members with leo-rpc
%% @private
send([],_StackedInfo,_BinObjs) ->
    ok;
send([#mdc_replication_info{cluster_members = Members}|Rest], StackedInfo, BinObjs) ->
    {ok, #?SYSTEM_CONF{cluster_id = ClusterId}} = leo_cluster_tbl_conf:get(),
    case send_1(Members, ClusterId, StackedInfo, BinObjs, 0) of
        {ok, RetL} ->
            %% Compare with local-cluster's objects
            compare_metadata(RetL);
        _ ->
            void
    end,
    send(Rest, StackedInfo, BinObjs).

%% @private
send_1([], ClusterId, StackedInfo,_BinObjs,_RetryTimes) ->
    ok = enqueue_fail_replication(StackedInfo, ClusterId),
    {error, ?ERROR_COULD_SEND_OBJ};
send_1([_|Rest], ClusterId, StackedInfo, BinObjs, ?DEF_MAX_RETRY_TIMES) ->
    send_1(Rest, ClusterId, StackedInfo, BinObjs, 0);
send_1([#?CLUSTER_MEMBER{node = Node,
                         port = Port}|Rest] = ClusterMembers,
       ClusterId, StackedInfo, BinObjs, RetryTimes) ->
    Node_1 = list_to_atom(lists:append([atom_to_list(Node),
                                        ":",
                                        integer_to_list(Port)])),
    case leo_rpc:call(Node_1, erlang, node, []) of
        Msg when Msg == timeout orelse
                 element(1, Msg) == badrpc ->
            send_1(ClusterMembers, ClusterId,
                   StackedInfo, BinObjs, RetryTimes + 1);
        _ ->
            Timeout = ?env_mdcr_req_timeout(),
            Ret = case leo_rpc:call(Node_1, ?MODULE, store,
                                    [ClusterId, BinObjs], Timeout) of
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
                    send_1(Rest, ClusterId,
                           StackedInfo, BinObjs, 0)
            end
    end.


%% @doc Put messages of fail replication into the queue
%% @private
enqueue_fail_replication([],_ClusterId) ->
    ok;
enqueue_fail_replication([{AddrId, Key}|Rest], ClusterId) ->
    QId = ?QUEUE_ID_SYNC_OBJ_WITH_DC,
    case leo_storage_mq:publish(QId, ClusterId, AddrId, Key) of
        ok ->
            void;
        {error, Cause} ->
            ?warn("enqueue_fail_replication/2",
                  [{qid, QId}, {cluster_id, ClusterId},
                   {addr_id, AddrId}, {key, Key}, {cause, Cause}])
    end,
    enqueue_fail_replication(Rest, ClusterId).
