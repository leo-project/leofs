%%======================================================================
%%
%% LeoFS Storage
%%
%% Copyright (c) 2012-2016 Rakuten, Inc.
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
%% @doc Handling an object, which is included in put, get, delete and head operation
%% @end
%%======================================================================
-module(leo_storage_handler_object).

-include("leo_storage.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_ordning_reda/include/leo_ordning_reda.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-undef(MAX_RETRY_TIMES).
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([get/1, get/2, get/3, get/4, get/5,
         put/1, put/2, put/4,
         delete/1, delete/2, delete/3,
         delete_objects_under_dir/1,
         delete_objects_under_dir/2,
         delete_objects_under_dir/3,
         head/2, head/3,
         head_with_calc_md5/3,
         replicate/1, replicate/3,
         prefix_search/3, prefix_search_and_remove_objects/1,
         find_uploaded_objects_by_key/1
        ]).

-define(REP_LOCAL, 'local').
-define(REP_REMOTE, 'remote').
-type(replication() :: ?REP_LOCAL | ?REP_REMOTE).

-define(DEF_DELIMITER, <<"/">>).

-ifdef(EUNIT).
-define(output_warn(Fun, _Key, _MSG), ok).
-else.
-define(output_warn(Fun, _Key, _MSG),
        ?warn(Fun, [{key, _Key}, {cause, _MSG}])).
-endif.


%%--------------------------------------------------------------------
%% API - GET
%%--------------------------------------------------------------------
%% @doc get object (from storage-node#1).
-spec(get(RefAndKey) ->
             {ok, reference(), binary(), binary(), binary()} |
             {error, reference(), any()} when RefAndKey::{reference(), binary()}).
get({Ref, Key}) ->
    ?debug("get/1", [{from, storage}, {method, get}, {key, Key}]),
    ok = leo_metrics_req:notify(?STAT_COUNT_GET),
    BeginTime = leo_date:clock(),
    case leo_redundant_manager_api:get_redundancies_by_key(get, Key) of
        {ok, #redundancies{id = AddrId}} ->
            IsForcedCheck = true,
            case get_fun(AddrId, Key, IsForcedCheck) of
                {ok, Metadata, #?OBJECT{data = Bin}} ->
                    ?access_log_storage_get(Key, byte_size(Bin), BeginTime, ok),
                    {ok, Ref, Metadata, Bin};
                {error, Cause} ->
                    ?access_log_storage_get(Key, 0, BeginTime, error),
                    ?error("get/1", [{from, storage}, {method, get},
                                     {key, Key}, {cause, Cause}]),
                    {error, Ref, Cause}
            end;
        _ ->
            Cause = ?ERROR_COULD_NOT_GET_REDUNDANCY,
            ?access_log_storage_get(Key, 0, BeginTime, {error, Cause}),
            ?error("get/1", [{from, storage}, {method, get},
                             {key, Key}, {cause, Cause}]),
            {error, Ref, Cause}
    end.

%% @doc get object (from storage-node#2).
-spec(get(ReadParams, Redundancies) ->
             {ok, #?METADATA{}, binary()} |
             {ok, match} |
             {error, any()} when ReadParams::#read_parameter{},
                                 Redundancies::[#redundant_node{}]).
get(ReadParameter, Redundancies) when Redundancies /= [] ->
    ok = leo_metrics_req:notify(?STAT_COUNT_GET),
    read_and_repair(ReadParameter, Redundancies);

get(#read_parameter{addr_id = AddrId} = ReadParameter,_Redundancies) ->
    BeginTime = leo_date:clock(),
    case leo_redundant_manager_api:get_redundancies_by_addr_id(get, AddrId) of
        {ok, #redundancies{nodes = Redundancies,
                           r = ReadQuorum}} ->
            get(ReadParameter#read_parameter{quorum = ReadQuorum},
                Redundancies);
        _Error ->
            Cause = ?ERROR_COULD_NOT_GET_REDUNDANCY,
            ?access_log_storage_get(ReadParameter#read_parameter.key, 0, BeginTime, {error, Cause}),
            ?error("get/2", [{from, storage}, {method, get},
                             {key, ReadParameter#read_parameter.key},
                             {cause, Cause}]),
            {error, Cause}
    end.

%% @doc Retrieve an object which is requested from gateway.
-spec(get(AddrId, Key, ReqId) ->
             {ok, #?METADATA{}, binary()} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 ReqId::integer()).
get(AddrId, Key, ReqId) ->
    BeginTime = leo_date:clock(),
    ?debug("get/3", [{from, gateway}, {method, get}, {key, Key}, {req_id, ReqId}]),
    Ret = get(#read_parameter{ref = make_ref(),
                              addr_id = AddrId,
                              key = Key,
                              req_id = ReqId}, []),
    case Ret of
        {ok, _, Bin} ->
            ?access_log_get(Key, byte_size(Bin), ReqId, BeginTime, ok);
        {error, Reply = not_found} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, error),
            ?error("get/3", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId}, {cause, Cause}])
    end,
    Ret.

%% @doc Retrieve an object which is requested from gateway w/etag.
-spec(get(AddrId, Key, ETag, ReqId) ->
             {ok, #?METADATA{}, binary()} |
             {ok, match} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 ETag::integer(),
                                 ReqId::integer()).
get(AddrId, Key, ETag, ReqId) ->
    BeginTime = leo_date:clock(),
    ?debug("get/4", [{from, gateway}, {method, get}, {key, Key}, {req_id, ReqId}, {etag, ETag}]),
    Ret = get(#read_parameter{ref = make_ref(),
                              addr_id = AddrId,
                              key = Key,
                              etag = ETag,
                              req_id = ReqId}, []),
    case Ret of
        {ok, match} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, match);
        {ok, _, Bin} ->
            ?access_log_get(Key, byte_size(Bin), ReqId, BeginTime, ok);
        {error, Reply = not_found} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, error),
            ?error("get/4", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId}, {etag, ETag}, {cause, Cause}])
    end,
    Ret.

%% @doc Retrieve a part of an object.
-spec(get(AddrId, Key, StartPos, EndPos, ReqId) ->
             {ok, #?METADATA{}, binary()} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer(),
                                 ReqId::integer()).
get(AddrId, Key, StartPos, EndPos, ReqId) ->
    BeginTime = leo_date:clock(),
    ?debug("get/5", [{from, gateway}, {method, get}, {key, Key}, {req_id, ReqId},
                     {start_pos, StartPos}, {end_pos, EndPos}]),
    Ret = get(#read_parameter{ref = make_ref(),
                              addr_id = AddrId,
                              key = Key,
                              start_pos = StartPos,
                              end_pos = EndPos,
                              req_id = ReqId}, []),
    case Ret of
        {ok, _, Bin} ->
            ?access_log_range_get(Key, StartPos, EndPos, byte_size(Bin), ReqId, BeginTime, ok);
        {error, Reply = not_found} ->
            ?access_log_get(Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_range_get(Key, StartPos, EndPos, 0, ReqId, BeginTime, error),
            ?error("get/5", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId},
                             {start_pos, StartPos}, {end_pos, EndPos}, {cause, Cause}])
    end,
    Ret.


%% @doc read data (common).
%% @private
-spec(get_fun(AddrId, Key, IsForcedCheck) ->
             {ok, #?METADATA{}, #?OBJECT{}} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 IsForcedCheck::boolean()).
get_fun(AddrId, Key, IsForcedCheck) ->
    get_fun(AddrId, Key, ?DEF_POS_START, ?DEF_POS_END, IsForcedCheck).

%% @private
-spec(get_fun(AddrId, Key, StartPos, EndPos) ->
             {ok, #?METADATA{}, #?OBJECT{}} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer()).
get_fun(AddrId, Key, StartPos, EndPos) ->
    get_fun(AddrId, Key, StartPos, EndPos, false).

%% @private
-spec(get_fun(AddrId, Key, StartPos, EndPos, IsForcedCheck) ->
             {ok, #?METADATA{}, #?OBJECT{}} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer(),
                                 IsForcedCheck::boolean()).
get_fun(AddrId, Key, StartPos, EndPos, IsForcedCheck) ->
    %% Check state of the node
    case leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS) of
        not_found ->
            %% Retrieve the object
            case leo_object_storage_api:get(
                   {AddrId, Key}, StartPos, EndPos, IsForcedCheck) of
                {ok, Metadata, Object} ->
                    {ok, Metadata, Object};
                not_found = Cause ->
                    {error, Cause};
                {error, ?ERROR_LOCKED_CONTAINER} ->
                    {error, unavailable};
                {error, Cause} ->
                    ok = leo_storage_watchdog_error:push(Cause),
                    {error, Cause}
            end;
        {ok, ErrorItems} ->
            ?debug("get_fun/4", "error-items:~p", [ErrorItems]),
            {error, unavailable}
    end.


%%--------------------------------------------------------------------
%% API - PUT
%%--------------------------------------------------------------------
%% @doc Insert an object (local replicate).
-spec(put(ObjAndRef) ->
             {ok, reference(), tuple()} |
             {error, reference(), any()} when ObjAndRef::{#?OBJECT{}, reference()}).
put({Object, Ref}) ->
    AddrId = Object#?OBJECT.addr_id,
    Key    = Object#?OBJECT.key,

    case Object#?OBJECT.del of
        ?DEL_TRUE->
            case leo_object_storage_api:head({AddrId, Key}) of
                {ok, MetaBin} ->
                    case binary_to_term(MetaBin) of
                        #?METADATA{cnumber = 0} ->
                            put_fun(Ref, AddrId, Key, Object);
                        #?METADATA{cnumber = CNumber} ->
                            case delete_chunked_objects(CNumber, Key) of
                                ok ->
                                    put_fun(Ref, AddrId, Key, Object);
                                {error, Cause} ->
                                    {error, Ref, Cause}
                            end;
                        _ ->
                            {error, Ref, 'invalid_data'}
                    end;
                not_found = Cause ->
                    {error, Ref, Cause};
                {error, Cause} ->
                    ok = leo_storage_watchdog_error:push(Cause),
                    {error, Ref, Cause}
            end;
        %% FOR PUT
        ?DEL_FALSE ->
            put_fun(Ref, AddrId, Key, Object)
    end.


%% @doc Insert an object (request from gateway).
-spec(put(Object, ReqId) ->
             {ok, ETag} | {error, any()} when Object::#?OBJECT{},
                                              ReqId::integer(),
                                              ETag::{etag, integer()}).
put(Object, ReqId) ->
    BeginTime = leo_date:clock(),
    ?debug("put/2", [{from, gateway}, {method, put}, {key, Object#?OBJECT.key}, {req_id, ReqId}]),
    ok = leo_metrics_req:notify(?STAT_COUNT_PUT),
    Ret = replicate_fun(?REP_LOCAL, ?CMD_PUT, Object#?OBJECT.addr_id,
                        Object#?OBJECT{method = ?CMD_PUT,
                                       clock = leo_date:clock(),
                                       req_id = ReqId}),
    case Ret of
        {ok, _} ->
            ?access_log_put(Object#?OBJECT.key, Object#?OBJECT.dsize, ReqId, BeginTime, ok);
        {error, Cause} ->
            ?access_log_put(Object#?OBJECT.key, Object#?OBJECT.dsize, ReqId, BeginTime, error),
            ?error("put/2", [{from, gateway}, {method, put},
                             {key, Object#?OBJECT.key}, {req_id, ReqId}, {cause, Cause}])
    end,
    Ret.


%% @doc Insert an  object (request from remote-storage-nodes/replicator).
-spec(put(Ref, From, Object, ReqId) ->
             {ok, atom()} |
             {error, any()} when Ref::reference(),
                                 From::pid(),
                                 Object::#?OBJECT{},
                                 ReqId::integer()).
put(Ref, From, Object, ReqId) ->
    BeginTime = leo_date:clock(),
    Method = case Object#?OBJECT.del of
                 ?DEL_TRUE ->
                     ok = leo_metrics_req:notify(?STAT_COUNT_DEL),
                     ?CMD_DELETE;
                 ?DEL_FALSE ->
                     ok = leo_metrics_req:notify(?STAT_COUNT_PUT),
                     ?CMD_PUT
             end,
    Key = Object#?OBJECT.key,
    ?debug("put/4", [{from, storage}, {method, Method}, {key, Key}, {req_id, ReqId}]),

    case replicate_fun(?REP_REMOTE, Method, Object) of
        {ok, ETag} ->
            ?access_log_storage_put(Method, Key, Object#?OBJECT.dsize, ReqId, BeginTime, ok),
            erlang:send(From, {Ref, {ok, ETag}});
        %% not found an object (during rebalance and delete-operation)
        {error, Cause = not_found} when ReqId == 0 ->
            ?access_log_storage_put(Method, Key, 0, ReqId, BeginTime, Cause),
            erlang:send(From, {Ref, {ok, 0}});
        {error, Cause} ->
            ?access_log_storage_put(Method, Key, 0, ReqId, BeginTime, error),
            ?error("put/4", [{from, storage}, {method, Method},
                             {key, Key}, {req_id, ReqId}, {cause, Cause}]),
            erlang:send(From, {Ref, {error, {node(), Cause}}})
    end.


%% Input an object into the object-storage
%% @private
-spec(put_fun(Ref, AddrId, Key, Object) ->
             {ok, reference(), tuple()} |
             {error, reference(), any()} when Ref::reference(),
                                              AddrId::integer(),
                                              Key::binary(),
                                              Object::#?OBJECT{}).
put_fun(Ref, AddrId, Key, #?OBJECT{del = ?DEL_TRUE} = Object) ->
    %% Check state of the node
    case leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS) of
        not_found ->
            %% Set deletion-flag to the object
            case leo_object_storage_api:delete({AddrId, Key}, Object) of
                ok ->
                    {ok, Ref, {etag, 0}};
                {error, ?ERROR_LOCKED_CONTAINER} ->
                    {error, Ref, unavailable};
                {error, Cause} ->
                    ok = leo_storage_watchdog_error:push(Cause),
                    {error, Ref, Cause}
            end;
        {ok, ErrorItems} ->
            ?debug("put_fun/4", "error-items:~p", [ErrorItems]),
            {error, Ref, unavailable}
    end;
put_fun(Ref, AddrId, Key, Object) ->
    %% Check state of the node
    case leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS) of
        not_found ->
            %% Put the object to the local object-storage
            case leo_object_storage_api:put({AddrId, Key}, Object) of
                {ok, ETag} ->
                    {ok, Ref, {etag, ETag}};
                {error, ?ERROR_LOCKED_CONTAINER} ->
                    {error, Ref, unavailable};
                {error, Cause} ->
                    ok = leo_storage_watchdog_error:push(Cause),
                    {error, Ref, Cause}
            end;
        {ok, ErrorItems} ->
            ?debug("put_fun/4", "error-items:~p", [ErrorItems]),
            {error, Ref, unavailable}
    end.



%% Remove chunked objects from the object-storage
%% @private
-spec(delete_chunked_objects(CIndex, ParentKey) ->
             ok | {error, any()} when CIndex::integer(),
                                      ParentKey::binary()).
delete_chunked_objects(0,_) ->
    ok;
delete_chunked_objects(CIndex, ParentKey) ->
    IndexBin = list_to_binary(integer_to_list(CIndex)),
    Key    = << ParentKey/binary, "\n", IndexBin/binary >>,
    AddrId = leo_redundant_manager_chash:vnode_id(Key),

    case delete(#?OBJECT{addr_id = AddrId,
                         key = Key,
                         cindex = CIndex,
                         clock = leo_date:clock(),
                         timestamp = leo_date:now(),
                         del = ?DEL_TRUE}, 0) of
        ok ->
            delete_chunked_objects(CIndex - 1, ParentKey);
        {error, Cause} ->
            {error, Cause}
    end.


%%--------------------------------------------------------------------
%% API - DELETE
%%--------------------------------------------------------------------
%% @doc Remove an object (request from storage)
-spec(delete(ObjAndRef) ->
             {ok, reference()} |
             {error, reference(), any()} when ObjAndRef::{#?OBJECT{}, reference()}).
delete({Object, Ref}) ->
    AddrId = Object#?OBJECT.addr_id,
    Key    = Object#?OBJECT.key,

    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            case catch binary_to_term(MetaBin) of
                {'EXIT', Cause} ->
                    {error, Cause};
                #?METADATA{del = ?DEL_TRUE} ->
                    {ok, Ref};
                #?METADATA{del = ?DEL_FALSE} ->
                    case leo_object_storage_api:delete(
                           {AddrId, Key}, Object#?OBJECT{data = <<>>,
                                                         dsize = 0,
                                                         del = ?DEL_TRUE}) of
                        ok ->
                            {ok, Ref};
                        {error, Why} ->
                            {error, Ref, Why}
                    end
            end;
        not_found = Cause ->
            {error, Ref, Cause};
        {error, Cause} ->
            ok = leo_storage_watchdog_error:push(Cause),
            {error, Ref, ?ERROR_COULD_NOT_GET_META}
    end.


%% @doc Remova an object (request from gateway)
-spec(delete(Object, ReqId) ->
             ok | {error, any()} when Object::#?OBJECT{},
                                      ReqId::integer()|reference()).
delete(Object, ReqId) ->
    delete(Object, ReqId, true).

-spec(delete(Object, ReqId, CheckUnderDir) ->
             ok | {error, any()} when Object::#?OBJECT{},
                                      ReqId::integer()|reference(),
                                      CheckUnderDir::boolean()).
delete(Object, ReqId, CheckUnderDir) ->
    BeginTime = leo_date:clock(),
    Key = Object#?OBJECT.key,
    ?debug("delete/3", [{from, gateway}, {method, del}, {key, Key}, {req_id, ReqId}]),
    ok = leo_metrics_req:notify(?STAT_COUNT_DEL),
    case replicate_fun(?REP_LOCAL, ?CMD_DELETE,
                       Object#?OBJECT.addr_id,
                       Object#?OBJECT{method = ?CMD_DELETE,
                                      data = <<>>,
                                      dsize = 0,
                                      clock = leo_date:clock(),
                                      req_id = ReqId,
                                      del = ?DEL_TRUE}) of
        {ok,_} ->
            ?access_log_delete(Key, Object#?OBJECT.dsize, ReqId, BeginTime, ok),
            delete_1(ok, Object, CheckUnderDir);
        {error, Cause = not_found} ->
            ?access_log_delete(Key, Object#?OBJECT.dsize, ReqId, BeginTime, Cause),
            delete_1({error, Cause}, Object, CheckUnderDir);
        {error, Cause} ->
            ?access_log_delete(Key, Object#?OBJECT.dsize, ReqId, BeginTime, error),
            ?error("delete/3", [{from, gateway}, {method, del},
                                {key, Object#?OBJECT.key}, {req_id, ReqId}, {cause, Cause}]),
            {error, Cause}
    end.

%% @private
delete_1(Ret,_Object, false) ->
    Ret;
delete_1(Ret, Object, true) ->
    ok = delete_objects_under_dir(Object),
    Ret.


%% Deletion object related constants
-define(BIN_SLASH, <<"/">>).
-define(BIN_NL,    <<"\n">>).

%% @doc Remove objects of the under directory
-spec(delete_objects_under_dir(Object) ->
             ok when Object::#?OBJECT{}).
delete_objects_under_dir(Object) ->
    Key = Object#?OBJECT.key,
    KSize = byte_size(Key),

    case catch binary:part(Key, (KSize - 1), 1) of
        {'EXIT',_} ->
            ok;
        ?BIN_SLASH ->
            leo_storage_mq:publish(?QUEUE_ID_DEL_DIR, Key);
        _ ->
            ok
    end.


%% @doc Remove objects of the under directory for remote-nodes
-spec(delete_objects_under_dir(Ref, Keys) ->
             {ok, Ref} when Ref::reference(),
                            Keys::[binary()|undefined]).
delete_objects_under_dir(Ref, []) ->
    {ok, Ref};
delete_objects_under_dir(Ref, [undefined|Rest]) ->
    delete_objects_under_dir(Ref, Rest);
delete_objects_under_dir(Ref, [Key|Rest]) ->
    _ = prefix_search_and_remove_objects(Key),
    delete_objects_under_dir(Ref, Rest).

-spec(delete_objects_under_dir(Nodes, Ref, Keys) ->
             {ok, Ref} when Nodes::[atom()],
                            Ref::reference(),
                            Keys::[binary()|undefined]).
delete_objects_under_dir([], Ref,_Keys) ->
    {ok, Ref};
delete_objects_under_dir([Node|Rest], Ref, Keys) when Node == erlang:node() ->
    {ok, Ref} = delete_objects_under_dir(Ref, Keys),
    delete_objects_under_dir(Rest, Ref, Keys);
delete_objects_under_dir([Node|Rest], Ref, Keys) ->
    case leo_misc:node_existence(Node) of
        true ->
            RPCKey = rpc:async_call(Node, leo_storage_mq,
                                    publish, [?QUEUE_ID_DEL_DIR, {bulk_insert, Keys}]),
            case rpc:nb_yield(RPCKey, ?DEF_REQ_TIMEOUT) of
                {value, ok} ->
                    ok;
                _Other ->
                    delete_objects_under_dir_1(Node, Keys)
            end;
        false ->
            delete_objects_under_dir_1(Node, Keys)
    end,
    delete_objects_under_dir(Rest, Ref, Keys).

%% @private
delete_objects_under_dir_1(_,[]) ->
    ok;
delete_objects_under_dir_1(Node, [Key|Rest]) ->
    QId = ?QUEUE_ID_DEL_DIR,
    ok = leo_storage_mq:publish(QId, Node, Key),
    delete_objects_under_dir_1(Node, Rest).


%%--------------------------------------------------------------------
%% API - HEAD
%%--------------------------------------------------------------------
%% @doc retrieve a meta-data from mata-data-server (file).
-spec(head(AddrId, Key) ->
             {ok, #?METADATA{}} | not_found | {error, any} when AddrId::integer(),
                                                                Key::binary()).
head(AddrId, Key) ->
    %% Do retry when being invoked as usual method
    head(AddrId, Key, true).

-spec(head(AddrId, Key, CanRetry) ->
             {ok, #?METADATA{}} | not_found | {error, any} when AddrId::integer(),
                                                                Key::binary(),
                                                                CanRetry::boolean()).
head(AddrId, Key, false) ->
    %% No retry when being invoked from recover/rebalance
    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            {ok, binary_to_term(MetaBin)};
        not_found = Error ->
            Error;
        {error, Cause} ->
            ok = leo_storage_watchdog_error:push(Cause),
            {error, Cause}
    end;
head(AddrId, Key, true) ->
    case leo_redundant_manager_api:get_redundancies_by_addr_id(get, AddrId) of
        {ok, #redundancies{nodes = Redundancies}} ->
            head_1(Redundancies, AddrId, Key);
        _ ->
            {error, ?ERROR_COULD_NOT_GET_REDUNDANCY}
    end.

%% @private
head_1([],_,_) ->
    {error, not_found};
head_1([#redundant_node{node = Node,
                        available = true}|Rest], AddrId, Key) when Node == erlang:node() ->
    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            {ok, binary_to_term(MetaBin)};
        Other ->
            case Other of
                not_found ->
                    void;
                {error, Cause} ->
                    ok = leo_storage_watchdog_error:push(Cause)
            end,
            head_1(Rest, AddrId, Key)
    end;
head_1([#redundant_node{node = Node,
                        available = true}|Rest], AddrId, Key) ->
    RPCKey = rpc:async_call(Node, leo_object_storage_api, head, [{AddrId, Key}]),
    case rpc:nb_yield(RPCKey, ?DEF_REQ_TIMEOUT) of
        {value, {ok, MetaBin}} ->
            {ok, binary_to_term(MetaBin)};
        _ ->
            head_1(Rest, AddrId, Key)
    end;
head_1([_|Rest], AddrId, Key) ->
    head_1(Rest, AddrId, Key).


%%--------------------------------------------------------------------
%% API - HEAD with calculating MD5
%%--------------------------------------------------------------------
%% @doc Retrieve a metada/data from backend_db/object-storage
%%      AND calc MD5 based on the body data
-spec(head_with_calc_md5(AddrId, Key, MD5Context) ->
             {ok, #?METADATA{}, any()} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 MD5Context::any()).
head_with_calc_md5(AddrId, Key, MD5Context) ->
    leo_object_storage_api:head_with_calc_md5({AddrId, Key}, MD5Context).


%%--------------------------------------------------------------------
%% API - COPY/STACK-SEND/RECEIVE-STORE
%%--------------------------------------------------------------------
%% @doc Replicate an object, which is requested from remote-cluster
-spec(replicate(Object) ->
             ok |
             {ok, reference()} |
             {error, reference()|any()} when Object::#?OBJECT{}).
replicate(Object) ->
    %% Transform an object to a metadata
    Metadata = leo_object_storage_transformer:object_to_metadata(Object),
    Method = case Object#?OBJECT.del of
                 ?DEL_TRUE ->
                     ?CMD_DELETE;
                 ?DEL_FALSE ->
                     ?CMD_PUT
             end,
    NumOfReplicas = Object#?OBJECT.num_of_replicas,
    AddrId = Metadata#?METADATA.addr_id,

    %% Retrieve redudancies
    case leo_redundant_manager_api:get_redundancies_by_addr_id(AddrId) of
        {ok, #redundancies{nodes = Redundancies,
                           w = WriteQuorum,
                           d = DeleteQuorum}} ->
            %% Replicate an object into the storage cluster
            Redundancies_1 = lists:sublist(Redundancies, NumOfReplicas),
            Quorum_1 = ?quorum(Method, WriteQuorum, DeleteQuorum),
            Quorum_2 = case (NumOfReplicas < Quorum_1) of
                           true when NumOfReplicas =< 1 ->
                               1;
                           true ->
                               NumOfReplicas - 1;
                           false ->
                               Quorum_1
                       end,
            case get_active_redundancies(Quorum_2, Redundancies_1) of
                {ok, Redundancies_2} ->
                    leo_storage_replicator:replicate(Method, Quorum_2, Redundancies_2,
                                                     Object, replicate_callback());
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Replicate an object from local to remote
-spec(replicate(DestNodes, AddrId, Key) ->
             ok |
             not_found |
             {error, any()} when DestNodes::[atom()],
                                 AddrId::integer(),
                                 Key::binary()).
replicate(DestNodes, AddrId, Key) ->
    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            Ref = make_ref(),
            case binary_to_term(MetaBin) of
                #?METADATA{del = ?DEL_FALSE} = Metadata ->
                    case ?MODULE:get({Ref, Key}) of
                        {ok, Ref, Metadata, Bin} ->
                            Ret = leo_sync_local_cluster:stack(
                                    DestNodes, AddrId, Key, Metadata, Bin),
                            Object_1 = leo_object_storage_transformer:metadata_to_object(Metadata),
                            Object_2 = Object_1#?OBJECT{method = ?CMD_PUT,
                                                        data = Bin},
                            ok = leo_sync_remote_cluster:defer_stack(Object_2),
                            Ret;
                        {error, Ref, Cause} ->
                            {error, Cause};
                        _Other ->
                            {error, invalid_response}
                    end;
                #?METADATA{del = ?DEL_TRUE} = Metadata ->
                    EmptyBin = <<>>,
                    Ret = leo_sync_local_cluster:stack(DestNodes, AddrId, Key, Metadata, EmptyBin),
                    Object_1 = leo_object_storage_transformer:metadata_to_object(Metadata),
                    Object_2 = Object_1#?OBJECT{method = ?CMD_DELETE,
                                                data = EmptyBin,
                                                dsize = 0,
                                                del = ?DEL_TRUE},
                    ok = leo_sync_remote_cluster:defer_stack(Object_2),
                    Ret;
                _ ->
                    {error, invalid_data_type}
            end;
        not_found = Error ->
            Error;
        {error, Cause} ->
            ok = leo_storage_watchdog_error:push(Cause),
            {error, Cause}
    end.


%%--------------------------------------------------------------------
%% API - Prefix Search (Fetch)
%%--------------------------------------------------------------------
prefix_search(ParentDir, Marker, MaxKeys) ->
    StartDateTime = leo_date:now(),
    Timeout = ?env_seeking_timeout_per_metadata() * MaxKeys,

    Fun = fun(Key, V, Acc) when length(Acc) =< MaxKeys ->
                  Now = leo_date:now(),
                  case (not_found ==
                            leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS)) of
                      true ->
                          case ((Now - StartDateTime) >= Timeout) of
                              true ->
                                  erlang:error(timeout);
                              false ->
                                  prefix_search_1(ParentDir, Marker, Key, V, Acc)
                          end;
                      false ->
                          erlang:error(?ERROR_SYSTEM_HIGH_LOAD)
                  end;
             (_, _, Acc) ->
                  Acc
          end,

    case catch leo_object_storage_api:fetch_by_key(
                 ParentDir, Fun) of
        {'EXIT', Cause} when is_tuple(Cause) ->
            {error, element(1, Cause)};
        {'EXIT', Cause} ->
            {error, Cause};
        Ret ->
            Ret
    end.


%% @private
prefix_search_1(ParentDir, Marker, Key, V, Acc) ->
    Meta_Pre = binary_to_term(V),
    case leo_object_storage_transformer:transform_metadata(Meta_Pre) of
        {error, Cause} ->
            ?error("prefix_search_1/5", [{key, Key}, {error, Cause}]),
            Acc;
        Meta ->
            prefix_search_2(ParentDir, Marker, Key, Meta, Acc)
    end.

prefix_search_2(ParentDir, Marker, Key, Meta, Acc) ->
    InRange = case Marker of
                  [] ->
                      true;
                  Key ->
                      false;
                  _  ->
                      (Marker == hd(lists:sort([Marker, Key])))
              end,
    Token_1 = leo_misc:binary_tokens(ParentDir, ?DEF_DELIMITER),
    Token_2 = leo_misc:binary_tokens(Key, ?DEF_DELIMITER),
    Length_1 = erlang:length(Token_1),
    Length_2 = Length_1 + 1,
    Length_3 = erlang:length(Token_2),
    IsChunkedObj = (nomatch /= binary:match(Key, <<"\n">>)),

    Pos_1 = case binary:match(Key, [ParentDir]) of
                nomatch ->
                    -1;
                {Pos, _} ->
                    Pos
            end,

    case (InRange == true
          andalso Pos_1 == 0) of
        true ->
            case (Length_3 - 1) of
                Length_1 when Meta#?METADATA.del == ?DEL_FALSE
                              andalso IsChunkedObj == false ->
                    KeyLen = byte_size(Key),

                    case (binary:part(Key, KeyLen - 1, 1) == ?DEF_DELIMITER
                          andalso KeyLen > 1) of
                        true  ->
                            case lists:keyfind(Key, 2, Acc) of
                                false ->
                                    ordsets:add_element(#?METADATA{key = Key,
                                                                   dsize = -1}, Acc);
                                _ ->
                                    Acc
                            end;
                        false ->
                            case lists:keyfind(Key, 2, Acc) of
                                false ->
                                    ordsets:add_element(Meta#?METADATA{offset = 0,
                                                                       ring_hash = 0}, Acc);
                                #?METADATA{clock = Clock} when Meta#?METADATA.clock > Clock ->
                                    Acc_1 = lists:keydelete(Key, 2, Acc),
                                    ordsets:add_element(Meta#?METADATA{offset = 0,
                                                                       ring_hash = 0}, Acc_1);
                                _ ->
                                    Acc
                            end
                    end;

                _Any when Meta#?METADATA.del == ?DEL_FALSE
                          andalso IsChunkedObj == false ->
                    {Token2, _} = lists:split(Length_2, Token_2),
                    Dir = lists:foldl(fun(Bin_1, <<>>) ->
                                              << Bin_1/binary,
                                                 ?DEF_DELIMITER/binary >>;
                                         (Bin_1, Bin_2) ->
                                              << Bin_2/binary,
                                                 Bin_1/binary,
                                                 ?DEF_DELIMITER/binary >>
                                      end, <<>>, Token2),
                    case lists:keyfind(Dir, 2, Acc) of
                        false ->
                            ordsets:add_element(#?METADATA{key   = Dir,
                                                           dsize = -1}, Acc);
                        _ ->
                            Acc
                    end;
                _ ->
                    Acc
            end;
        false ->
            Acc
    end.


%% @doc Retrieve object of deletion from object-storage by key
-spec(prefix_search_and_remove_objects(ParentDir) ->
             {ok, [_]} |
             not_found when ParentDir::undefined|binary()).
prefix_search_and_remove_objects(undefined) ->
    not_found;
prefix_search_and_remove_objects(ParentDir) ->
    Fun = fun(Key, V, Acc) ->
                  Metadata = binary_to_term(V),
                  AddrId = Metadata#?METADATA.addr_id,
                  Pos_1 = case binary:match(Key, [ParentDir]) of
                              nomatch ->
                                  -1;
                              {Pos,_} ->
                                  Pos
                          end,

                  case (Pos_1 == 0) of
                      true when Metadata#?METADATA.del == ?DEL_FALSE ->
                          QId = ?QUEUE_ID_ASYNC_DELETION,
                          case leo_storage_mq:publish(QId, AddrId, Key) of
                              ok ->
                                  void;
                              {error, Cause} ->
                                  ?warn("prefix_search_and_remove_objects/1",
                                        [{qid, QId}, {addr_id, AddrId},
                                         {key, Key}, {cause, Cause}])
                          end,
                          Acc;
                      _ ->
                          Acc
                  end
          end,
    case catch leo_object_storage_api:fetch_by_key(ParentDir, Fun) of
        {'EXIT', Cause} ->
            {error, Cause};
        Ret ->
            Ret
    end.


%% @doc Find already uploaded objects by original-filename
-spec(find_uploaded_objects_by_key(OriginalKey) ->
             {ok, list()} | not_found when OriginalKey::binary()).
find_uploaded_objects_by_key(OriginalKey) ->
    Fun = fun(Key, V, Acc) ->
                  Metadata       = binary_to_term(V),

                  case (nomatch /= binary:match(Key, <<"\n">>)) of
                      true ->
                          Pos_1 = case binary:match(Key, [OriginalKey]) of
                                      nomatch   -> -1;
                                      {Pos, _} -> Pos
                                  end,
                          case (Pos_1 == 0) of
                              true ->
                                  [Metadata|Acc];
                              false ->
                                  Acc
                          end;
                      false ->
                          Acc
                  end
          end,
    case catch leo_object_storage_api:fetch_by_key(
                 OriginalKey, Fun) of
        {'EXIT', Cause} ->
            {error, Cause};
        Ret ->
            Ret
    end.


%%--------------------------------------------------------------------
%% INNNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Retrieve active redundancies
%% @private
-spec(get_active_redundancies(Quorum, Redundancies) ->
             {ok, [#redundant_node{}]} |
             {error, any()} when Quorum::non_neg_integer(),
                                 Redundancies::[#redundant_node{}]).
get_active_redundancies(_, []) ->
    {error, ?ERROR_NOT_SATISFY_QUORUM};
get_active_redundancies(Quorum, Redundancies) ->
    AvailableNodes = [RedundantNode ||
                         #redundant_node{available = true} = RedundantNode <- Redundancies],
    case (Quorum =< erlang:length(AvailableNodes)) of
        true ->
            {ok, AvailableNodes};
        false ->
            {error, ?ERROR_NOT_SATISFY_QUORUM}
    end.


%% @doc read reapir - compare with remote-node's meta-data.
%% @private
-spec(read_and_repair(ReadParams, Redundancies) ->
             {ok, #?METADATA{}, binary()} |
             {ok, match} |
             {error, any()} when ReadParams::#read_parameter{},
                                 Redundancies::[#redundant_node{}]).
read_and_repair(#read_parameter{quorum = Q} = ReadParams, Redundancies) ->
    case get_active_redundancies(Q, Redundancies) of
        {ok, AvailableNodes} ->
            read_and_repair_1(ReadParams, AvailableNodes, AvailableNodes, []);
        Error ->
            Error
    end.

%% @private
read_and_repair_1(_,[],_,[Error|_]) ->
    {error, Error};
read_and_repair_1(ReadParams, [Node|Rest], AvailableNodes, Errors) ->
    case read_and_repair_2(ReadParams, Node, AvailableNodes) of
        {error, Cause} ->
            read_and_repair_1(ReadParams, Rest, AvailableNodes, [Cause|Errors]);
        Ret ->
            Ret
    end.

%% @private
-spec(read_and_repair_2(ReadParams, Redundancies, Redundancies) ->
             {ok, #?METADATA{}, binary()} |
             {ok, match} |
             {error, any()} when ReadParams::#read_parameter{},
                                 Redundancies::[atom()]).
read_and_repair_2(_, [], _) ->
    {error, not_found};
read_and_repair_2(#read_parameter{addr_id = AddrId,
                                  key = Key,
                                  etag = 0,
                                  start_pos = StartPos,
                                  end_pos = EndPos} = ReadParameter,
                  #redundant_node{node = Node}, Redundancies) when Node == erlang:node() ->
    read_and_repair_3(
      get_fun(AddrId, Key, StartPos, EndPos), ReadParameter, Redundancies);

read_and_repair_2(#read_parameter{addr_id = AddrId,
                                  key = Key,
                                  etag = ETag,
                                  start_pos = StartPos,
                                  end_pos = EndPos} = ReadParameter,
                  #redundant_node{node = Node}, Redundancies) when Node == erlang:node() ->
    %% Retrieve an head of object,
    %%     then compare it with requested 'Etag'
    HeadRet = case leo_object_storage_api:head({AddrId, Key}) of
                  {ok, MetaBin} ->
                      Metadata = binary_to_term(MetaBin),
                      case Metadata#?METADATA.checksum of
                          ETag ->
                              {ok, match};
                          _ ->
                              []
                      end;
                  not_found ->
                      [];
                  {error, Cause} ->
                      ok = leo_storage_watchdog_error:push(Cause),
                      []
              end,

    %% If the result is 'match', then response it,
    %% not the case, retrieve an object by key
    case HeadRet of
        {ok, match} = Reply ->
            Reply;
        _ ->
            read_and_repair_3(
              get_fun(AddrId, Key, StartPos, EndPos), ReadParameter, Redundancies)
    end;

read_and_repair_2(ReadParameter, #redundant_node{node = Node}, Redundancies) ->
    Ref = make_ref(),
    Key = ReadParameter#read_parameter.key,

    RPCKey = rpc:async_call(Node, ?MODULE, get, [{Ref, Key}]),
    RetRPC = case catch rpc:nb_yield(RPCKey, ?DEF_REQ_TIMEOUT) of
                 {'EXIT', Cause} ->
                     {error, Cause};
                 {value, {ok, Ref, Meta, Bin}} ->
                     {ok, Meta, #?OBJECT{data = Bin}};
                 {value, {error, Ref, Cause}} ->
                     {error, Cause};
                 {value, {badrpc, Cause}} ->
                     {error, Cause};
                 {value, _} ->
                     {error, invalid_access};
                 timeout = Cause ->
                     {error, Cause}
             end,
    read_and_repair_3(RetRPC, ReadParameter, Redundancies).

%% @private
read_and_repair_3({ok, Metadata, #?OBJECT{data = Bin}}, #read_parameter{}, []) ->
    {ok, Metadata, Bin};
read_and_repair_3({ok, match} = Reply, #read_parameter{},_Redundancies) ->
    Reply;
read_and_repair_3({ok, Metadata, #?OBJECT{data = Bin}},
                  #read_parameter{quorum = Quorum} = ReadParameter, Redundancies) ->
    Fun = fun(ok) ->
                  {ok, Metadata, Bin};
             ({error, unavailable} = Ret) ->
                  Ret;
             ({error,_Cause}) ->
                  {error, ?ERROR_RECOVER_FAILURE}
          end,
    ReadParameter_1 = ReadParameter#read_parameter{quorum = Quorum},
    leo_storage_read_repairer:repair(ReadParameter_1, Redundancies, Metadata, Fun);

read_and_repair_3({error, not_found = Cause}, #read_parameter{key = _K}, _Redundancies) ->
    {error, Cause};
read_and_repair_3({error, timeout = Cause}, #read_parameter{key = _K}, _Redundancies) ->
    ?output_warn("read_and_repair_3/3", _K, Cause),
    {error, Cause};
read_and_repair_3({error, Cause}, #read_parameter{key = _K}, _Redundancies) ->
    ?output_warn("read_and_repair_3/3", _K, Cause),
    {error, Cause};
read_and_repair_3(_,_,_) ->
    {error, invalid_request}.


%% @doc Replicate an object from local-node to remote node
%% @private
-spec(replicate_fun(replication(), request_verb(), integer(), #?OBJECT{}) ->
             {ok, ETag} | {error, any()} when ETag::{etag, integer()}).
replicate_fun(?REP_LOCAL, Method, AddrId, Object) ->
    %% Check state of the node
    case leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS) of
        not_found ->
            case leo_redundant_manager_api:get_redundancies_by_addr_id(put, AddrId) of
                {ok, #redundancies{nodes = Redundancies,
                                   w = WriteQuorum,
                                   d = DeleteQuorum,
                                   ring_hash = RingHash}} ->
                    Quorum = ?quorum(Method, WriteQuorum, DeleteQuorum),
                    case get_active_redundancies(Quorum, Redundancies) of
                        {ok, Redundancies_1} ->
                            leo_storage_replicator:replicate(
                              Method, Quorum, Redundancies_1,
                              Object#?OBJECT{ring_hash = RingHash},
                              replicate_callback(Object));
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error,_Cause} ->
                    {error, ?ERROR_COULD_NOT_GET_REDUNDANCY}
            end;
        {ok, ErrorItems}->
            ?debug("replicate_fun/4", "error-items:~p", [ErrorItems]),
            {error, unavailable}
    end.

%% @doc obj-replication request from remote node.
%%
replicate_fun(?REP_REMOTE, Method, Object) ->
    Ref = make_ref(),
    Ret = case Method of
              ?CMD_PUT ->
                  ?MODULE:put({Object, Ref});
              ?CMD_DELETE ->
                  ?MODULE:delete({Object, Ref})
          end,
    case Ret of
        %% for put-operation
        {ok, Ref, Checksum} ->
            {ok, Checksum};
        %% for delete-operation
        {ok, Ref} ->
            {ok, 0};
        {error, Ref, not_found = Cause} ->
            {error, Cause};
        {error, Ref, unavailable = Cause} ->
            {error, Cause};
        {error, Ref, Cause} ->
            ?warn("replicate_fun/3", [{cause, Cause}]),
            {error, Cause}
    end.


%% @doc Being callback, after executed replication of an object
%% @private
-spec(replicate_callback() ->
             function()).
replicate_callback() ->
    replicate_callback(null).

-spec(replicate_callback(#?OBJECT{}|null) ->
             function()).
replicate_callback(Object) ->
    fun({ok, ?CMD_PUT, Checksum}) ->
            ok = leo_sync_remote_cluster:defer_stack(Object),
            {ok, Checksum};
       ({ok,?CMD_DELETE,_Checksum}) ->
            ok = leo_sync_remote_cluster:defer_stack(Object),
            {ok, 0};
       ({error, unavailable} = Ret) ->
            Ret;
       ({error, Errors}) ->
            case catch lists:keyfind(not_found, 2, Errors) of
                {'EXIT',_} ->
                    {error, ?ERROR_REPLICATE_FAILURE};
                false ->
                    {error, ?ERROR_REPLICATE_FAILURE};
                _ ->
                    {error, not_found}
            end
    end.
