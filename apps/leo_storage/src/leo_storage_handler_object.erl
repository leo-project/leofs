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
%% @doc Handling an object, which is included in put, get, delete and head operation
%% @end
%%======================================================================
-module(leo_storage_handler_object).

-include("leo_storage.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_mq/include/leo_mq.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_ordning_reda/include/leo_ordning_reda.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-undef(MAX_RETRY_TIMES).
-include_lib("leo_statistics/include/leo_statistics.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([get/1, get/2, get/3, get/4, get/5,
         put/1, put/2, put/3, put/4,
         delete/1, delete/2, delete/3, delete/4,
         delete_objects_under_dir/1,
         head/2, head/3,
         head_with_calc_md5/3,
         replicate/1, replicate/3,
         prefix_search/3, prefix_search_and_remove_objects/3,
         find_uploaded_objects_by_key/1,
         is_key_under_del_dir/1, can_compact_object/2
        ]).
-export([debug_put/3, debug_delete/2]).

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
             {ok, Ref, Metadata, Bin} |
             {error, Ref, Cause} when Ref::reference(),
                                      Key::binary(),
                                      RefAndKey::{Ref, Key},
                                      Metadata::#?METADATA{},
                                      Bin::binary(),
                                      Cause::any()).
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
                    ?access_log_storage_get(?ACC_LOG_L_ERROR, Key, 0, BeginTime, error),
                    ?error("get/1", [{from, storage}, {method, get},
                                     {key, Key}, {cause, Cause}]),
                    {error, Ref, Cause}
            end;
        _ ->
            Cause = ?ERROR_COULD_NOT_GET_REDUNDANCY,
            ?access_log_storage_get(?ACC_LOG_L_ERROR, Key, 0, BeginTime, {error, Cause}),
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
    case read_and_repair(ReadParameter, Redundancies) of
        {ok, #?METADATA{meta = CMeta} = Meta, Bin} when CMeta =/= <<>> ->
            {ok, NewMeta} = get_cmeta(Meta),

            {ok, NewMeta, Bin};
        Other ->
            Other
    end;

get(#read_parameter{addr_id = AddrId} = ReadParameter,_Redundancies) ->
    BeginTime = leo_date:clock(),
    case leo_redundant_manager_api:get_redundancies_by_addr_id(get, AddrId) of
        {ok, #redundancies{nodes = Redundancies,
                           r = ReadQuorum}} ->
            get(ReadParameter#read_parameter{quorum = ReadQuorum},
                Redundancies);
        _Error ->
            Cause = ?ERROR_COULD_NOT_GET_REDUNDANCY,
            ?access_log_storage_get(?ACC_LOG_L_ERROR, ReadParameter#read_parameter.key, 0,
                                    BeginTime, {error, Cause}),
            ?error("get/2", [{from, storage}, {method, get},
                             {key, ReadParameter#read_parameter.key},
                             {cause, Cause}]),
            {error, Cause}
    end.

%% @doc Retrieve an object which is requested from gateway.
-spec(get(AddrId, Key, ReqId) ->
             {ok, Metadata, Bin} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 ReqId::integer(),
                                 Metadata::#?METADATA{},
                                 Bin::binary()).
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
            ?access_log_get(?ACC_LOG_L_ERROR, Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_get(?ACC_LOG_L_ERROR, Key, 0, ReqId, BeginTime, error),
            ?error("get/3", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId}, {cause, Cause}])
    end,
    Ret.

%% @doc Retrieve an object which is requested from gateway w/etag.
-spec(get(AddrId, Key, ETag, ReqId) ->
             {ok, Metadata, Bin} |
             {ok, match} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 ETag::integer(),
                                 ReqId::integer(),
                                 Metadata::#?METADATA{},
                                 Bin::binary()).
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
            ?access_log_get(?ACC_LOG_L_ERROR, Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_get(?ACC_LOG_L_ERROR, Key, 0, ReqId, BeginTime, error),
            ?error("get/4", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId}, {etag, ETag}, {cause, Cause}])
    end,
    Ret.

%% @doc Retrieve a part of an object.
-spec(get(AddrId, Key, StartPos, EndPos, ReqId) ->
             {ok, Metadata, Bin} |
             {error, any()} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer(),
                                 ReqId::integer(),
                                 Metadata::#?METADATA{},
                                 Bin::binary()).
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
            ?access_log_get(?ACC_LOG_L_ERROR, Key, 0, ReqId, BeginTime, Reply);
        {error, Cause} ->
            ?access_log_range_get(?ACC_LOG_L_ERROR, Key, StartPos, EndPos, 0, ReqId, BeginTime, error),
            ?error("get/5", [{from, gateway}, {method, get},
                             {key, Key}, {req_id, ReqId},
                             {start_pos, StartPos}, {end_pos, EndPos}, {cause, Cause}])
    end,
    Ret.

%% @doc retrieve UDM and set it back to the meta field
%%      to fix https://github.com/leo-project/leofs/issues/641
%% @private
-spec(get_cmeta(Metadata) ->
             {ok, Metadata} |
             {error, any()} when Metadata::#?METADATA{}).
get_cmeta(#?METADATA{key = Key,
                     meta = CMeta} = Metadata) ->
    case leo_object_storage_transformer:get_udm_from_cmeta_bin(CMeta) of
        {ok, {ok, {ok, UDM}}} ->
            %% match if an object belongs to the destination and already read_repaired.
            {ok, Metadata#?METADATA{meta = term_to_binary(UDM)}};
        {ok, {ok, UDM}} ->
            %% match if an object belongs to the destination and still not read_repaired.
            {ok, Metadata#?METADATA{meta = term_to_binary(UDM)}};
        {ok, []} ->
            %% match if an object belongs to the soruce
            %% match with <= 1.3.2.1
            {ok, Metadata};
        {ok, UDM} ->
            %% match if an object belongs to the source
            %% match with >= 1.3.3
            {ok, Metadata#?METADATA{meta = term_to_binary(UDM)}};
        Other ->
            %% ignore broken custom metadata and just log the error
            ?error("get_cmeta/1", [{from, storage}, {method, get},
                                   {key, Key}, {meta, CMeta}, {cause, Other}]),
            {ok, Metadata#?METADATA{meta = <<>>}}
    end.

%% @doc read data (common).
%% @private
-spec(get_fun(AddrId, Key, IsForcedCheck) ->
             {ok, Metadata, Object} |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 IsForcedCheck::boolean(),
                                 Metadata::#?METADATA{},
                                 Object::#?OBJECT{},
                                 Cause::any()).
get_fun(AddrId, Key, IsForcedCheck) ->
    get_fun(AddrId, Key, ?DEF_POS_START, ?DEF_POS_END, IsForcedCheck).

%% @private
-spec(get_fun(AddrId, Key, StartPos, EndPos) ->
             {ok, Metadata, Object} |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer(),
                                 Metadata::#?METADATA{},
                                 Object::#?OBJECT{},
                                 Cause::any()).
get_fun(AddrId, Key, StartPos, EndPos) ->
    get_fun(AddrId, Key, StartPos, EndPos, false).

%% @private
-spec(get_fun(AddrId, Key, StartPos, EndPos, IsForcedCheck) ->
             {ok, Metadata, Object} |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 StartPos::integer(),
                                 EndPos::integer(),
                                 IsForcedCheck::boolean(),
                                 Metadata::#?METADATA{},
                                 Object::#?OBJECT{},
                                 Cause::any()).
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
             {ok, Ref, EtagRet} |
             {error, Ref, Cause} when Ref::reference(),
                                      Object::#?OBJECT{},
                                      ObjAndRef::{Object, Ref},
                                      EtagRet::etag_ret(),
                                      Cause::any()).
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
    put(Object, ReqId, gateway).

-spec(put(Object, ReqId, From) ->
             {ok, ETag} | {error, any()} when Object::#?OBJECT{},
                                              ReqId::integer(),
                                              From::atom(),
                                              ETag::{etag, integer()}).
put(Object, ReqId, From) ->
    BeginTime = leo_date:clock(),
    ?debug("put/2", [{from, From}, {method, put}, {key, Object#?OBJECT.key}, {req_id, ReqId}]),
    ok = leo_metrics_req:notify(?STAT_COUNT_PUT),
    Ret = replicate_fun(?REP_LOCAL, ?CMD_PUT, Object#?OBJECT.addr_id,
                        Object#?OBJECT{method = ?CMD_PUT,
                                       clock = leo_date:clock(),
                                       req_id = ReqId}, gateway),
    case Ret of
        {ok, _} ->
            ?access_log_put(Object#?OBJECT.key, Object#?OBJECT.dsize, ReqId, BeginTime, ok);
        {error, Cause} ->
            ?access_log_put(?ACC_LOG_L_ERROR, Object#?OBJECT.key, Object#?OBJECT.dsize, ReqId, BeginTime, error),
            ?error("put/2", [{from, From}, {method, put},
                             {key, Object#?OBJECT.key}, {req_id, ReqId}, {cause, Cause}])
    end,
    Ret.

%% @doc Insert an  object (request from remote-storage-nodes/replicator).
-spec(put(Ref, From, Object, ReqId) ->
             {ok, Etag} |
             {error, Cause} when Ref::reference(),
                                 From::pid(),
                                 Object::#?OBJECT{},
                                 ReqId::integer(),
                                 Etag::non_neg_integer(),
                                 Cause::any()).
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
            ?access_log_storage_put(?ACC_LOG_L_ERROR, Method, Key, 0, ReqId, BeginTime, Cause),
            erlang:send(From, {Ref, {ok, 0}});
        {error, Cause} ->
            ?access_log_storage_put(?ACC_LOG_L_ERROR, Method, Key, 0, ReqId, BeginTime, error),
            ?error("put/4", [{from, storage}, {method, Method},
                             {key, Key}, {req_id, ReqId}, {cause, Cause}]),
            erlang:send(From, {Ref, {error, {node(), Cause}}})
    end.


%% Input an object into the object-storage
%% @private
-spec(put_fun(Ref, AddrId, Key, Object) ->
             {ok, Ref, EtagRet} |
             {error, Ref, Cause} when Ref::reference(),
                                      AddrId::integer(),
                                      Key::binary(),
                                      Object::#?OBJECT{},
                                      EtagRet::etag_ret(),
                                      Cause::any()).
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
             ok | {error, Cause} when CIndex::integer(),
                                      ParentKey::binary(),
                                      Cause::any()).
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
                         del = ?DEL_TRUE}, 0, false, storage) of
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
             {ok, Ref} |
             {error, Ref, Cause} when Ref::reference(),
                                      Object::#?OBJECT{},
                                      ObjAndRef::{Object, Ref},
                                      Cause::any()).
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
             ok | {error, Cause} when Object::#?OBJECT{},
                                      ReqId::integer()|reference(),
                                      Cause::any()).
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
                                      del = ?DEL_TRUE}, gateway) of
        {ok,_} ->
            ?access_log_delete(Key, Object#?OBJECT.dsize, ReqId, BeginTime, ok),
            delete_1(ok, Object, CheckUnderDir);
        {error, Cause = not_found} ->
            ?access_log_delete(?ACC_LOG_L_ERROR, Key, Object#?OBJECT.dsize, ReqId, BeginTime, Cause),
            delete_1({error, Cause}, Object, CheckUnderDir);
        {error, Cause} ->
            ?access_log_delete(?ACC_LOG_L_ERROR, Key, Object#?OBJECT.dsize, ReqId, BeginTime, error),
            ?error("delete/3", [{from, gateway}, {method, del},
                                {key, Object#?OBJECT.key}, {req_id, ReqId}, {cause, Cause}]),
            {error, Cause}
    end.

%% @doc Remova an object (request from leo_mq/storage)
-spec(delete(Object, ReqId, CheckUnderDir, From) ->
             ok | {error, any()} when Object::#?OBJECT{},
                                      ReqId::integer()|reference(),
                                      CheckUnderDir::boolean(),
                                      From::atom()).
delete(Object, ReqId, CheckUnderDir, From) ->
    Key = Object#?OBJECT.key,
    ?debug("delete/3", [{from, From}, {method, del}, {key, Key}, {req_id, ReqId}]),
    case replicate_fun(?REP_LOCAL, ?CMD_DELETE,
                       Object#?OBJECT.addr_id,
                       Object#?OBJECT{method = ?CMD_DELETE,
                                      data = <<>>,
                                      dsize = 0,
                                      clock = leo_date:clock(),
                                      req_id = ReqId,
                                      del = ?DEL_TRUE}, From) of
        {ok,_} ->
            delete_1(ok, Object, CheckUnderDir);
        {error, Cause = not_found} ->
            delete_1({error, Cause}, Object, CheckUnderDir);
        {error, Cause} ->
            ?debug("delete/4", [{from, From}, {method, del},
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
            leo_storage_handler_del_directory:enqueue(?TYPE_DEL_DIR, Key);
        ?BIN_NL ->
            leo_storage_handler_del_directory:enqueue(?TYPE_DEL_DIR, Key);
        _ ->
            ok
    end.


%%--------------------------------------------------------------------
%% API - HEAD
%%--------------------------------------------------------------------
%% @doc retrieve a meta-data from mata-data-server (file).
-spec(head(AddrId, Key) ->
             {ok, Metadata} |
             not_found |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 Metadata::#?METADATA{},
                                 Cause::any()).
head(AddrId, Key) ->
    %% Do retry when being invoked as usual method
    head(AddrId, Key, true).

-spec(head(AddrId, Key, CanRetry) ->
             {ok, Metadata} |
             not_found |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 CanRetry::boolean(),
                                 Metadata::#?METADATA{},
                                 Cause::any()).
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
            Meta = binary_to_term(MetaBin),
            case Meta#?METADATA.meta of
                <<>> ->
                    {ok, Meta};
                _CMeta ->
                    get_cmeta(Meta)
            end;
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
            Meta = binary_to_term(MetaBin),
            case Meta#?METADATA.meta of
                <<>> ->
                    {ok, Meta};
                _CMeta ->
                    get_cmeta(Meta)
            end;
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
             {ok, Metadata, NewHashContext} |
             {error, Cause} when AddrId::integer(),
                                 Key::binary(),
                                 MD5Context::any(),
                                 Metadata::#?METADATA{},
                                 NewHashContext::binary(),
                                 Cause::any()).
head_with_calc_md5(AddrId, Key, MD5Context) ->
    leo_object_storage_api:head_with_calc_md5({AddrId, Key}, MD5Context).


%%--------------------------------------------------------------------
%% API - COPY/STACK-SEND/RECEIVE-STORE
%%--------------------------------------------------------------------
%% @doc Replicate an object, which is requested from remote-cluster
-spec(replicate(Object) ->
             {ok, Etag} |
             {error, Cause} when Object::#?OBJECT{},
                                 Etag::non_neg_integer(),
                                 Cause::any()).
replicate(#?OBJECT{num_of_replicas = Preferred_N,
                   preferred_w = Preferred_W,
                   preferred_d = Preferred_D,
                   del = DelFlag} = Object) ->
    %% Transform an object to a metadata
    Metadata = leo_object_storage_transformer:object_to_metadata(Object),
    Method = case DelFlag of
                 ?DEL_TRUE ->
                     ?CMD_DELETE;
                 ?DEL_FALSE ->
                     ?CMD_PUT
             end,
    AddrId = Metadata#?METADATA.addr_id,

    %% Retrieve redudancies
    case leo_redundant_manager_api:get_redundancies_by_addr_id(AddrId) of
        {ok, #redundancies{nodes = Redundancies,
                           n = N,
                           w = W,
                           d = D}} ->
            %% Retrieve each quorum, 'w' and 'd'.
            %% If 'object' contains 'preferred_w' and 'preferred_d',
            %% those quorums are adopted instead of the quorums of the local consistency level.
            W_1 = case (Preferred_W < 1) of
                      true ->
                          W;
                      false ->
                          Preferred_W
                  end,
            D_1 = case (Preferred_D < 1) of
                      true ->
                          D;
                      false ->
                          Preferred_D
                  end,

            %% Replicate an object into the storage cluster
            {NumOfReplicas, Redundancies_1} =
                case (N > Preferred_N andalso Preferred_N > 0) of
                    true ->
                        {Preferred_N, lists:sublist(Redundancies, Preferred_N)};
                    false ->
                        {N, Redundancies}
                end,
            Quorum_1 = ?quorum(Method, W_1, D_1),
            Quorum_2 = case (NumOfReplicas < Quorum_1) of
                           true when NumOfReplicas =< 1 ->
                               1;
                           true ->
                               NumOfReplicas - 1;
                           false ->
                               Quorum_1
                       end,
            leo_storage_replicator:replicate(Method, Quorum_2, Redundancies_1,
                                             Object, replicate_callback());
        {error, Cause} ->
            {error, Cause}
    end.


%% @doc Fix an inconsistent object
%%       [leo_storage_mq:correct_redundancies/1
%%       => leo_storage_api:synchronize/2
%%       => this function is called]
-spec(replicate(InconsistentNodes, AddrId, Key) ->
             ok |
             not_found |
             {error, Cause} when InconsistentNodes::[node()],
                                 AddrId::integer(),
                                 Key::binary(),
                                 Cause::any()).
replicate(InconsistentNodes, AddrId, Key) ->
    case leo_object_storage_api:head({AddrId, Key}) of
        {ok, MetaBin} ->
            case binary_to_term(MetaBin) of
                #?METADATA{num_of_replicas = Preferred_N,
                           del = DelFlag} = Metadata ->
                    Ref = make_ref(),
                    InconsistentNodes_1 =
                        get_inconsistent_nodes(AddrId, Preferred_N, InconsistentNodes),

                    case DelFlag of
                        ?DEL_FALSE ->
                            case ?MODULE:get({Ref, Key}) of
                                {ok, Ref, Metadata, Bin} ->
                                    Ret = leo_sync_local_cluster:stack(
                                            InconsistentNodes_1, AddrId, Key, Metadata, Bin),
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
                        ?DEL_TRUE ->
                            EmptyBin = <<>>,
                            Ret = leo_sync_local_cluster:stack(
                                    InconsistentNodes_1, AddrId, Key, Metadata, EmptyBin),
                            Object_1 = leo_object_storage_transformer:metadata_to_object(Metadata),
                            Object_2 = Object_1#?OBJECT{method = ?CMD_DELETE,
                                                        data = EmptyBin,
                                                        dsize = 0,
                                                        del = ?DEL_TRUE},
                            ok = leo_sync_remote_cluster:defer_stack(Object_2),
                            Ret
                    end;
                _ ->
                    {error, invalid_data_type}
            end;
        not_found = Error ->
            Error;
        {error, Cause} ->
            ok = leo_storage_watchdog_error:push(Cause),
            {error, Cause}
    end.


%% @private
-spec(get_inconsistent_nodes(AddrId, Preferred_N, InconsistentNodes) ->
             [Node] | {error, Cause} when AddrId::integer(),
                                          Preferred_N::non_neg_integer(),
                                          Node::node(),
                                          InconsistentNodes::[Node],
                                          Cause::any()).
get_inconsistent_nodes(AddrId, Preferred_N, InconsistentNodes) ->
    case leo_redundant_manager_api:get_redundancies_by_addr_id(AddrId) of
        {ok, #redundancies{nodes = Redundancies,
                           n = N}} ->
            case (N > Preferred_N andalso Preferred_N > 0) of
                true ->
                    Redundancies_1 = lists:sublist(Redundancies, Preferred_N),
                    lists:reverse(
                      lists:foldl(
                        fun(Node, Acc) ->
                                case lists:filter(
                                       fun(#redundant_node{node = RedundantNode}) ->
                                               Node == RedundantNode
                                       end, Redundancies_1) of
                                    [] ->
                                        Acc;
                                    _ ->
                                        [Node|Acc]
                                end
                        end, [], InconsistentNodes));
                false ->
                    InconsistentNodes
            end;
        {error, Cause} ->
            {error, Cause}
    end.


%%--------------------------------------------------------------------
%% API - Prefix Search (Fetch)
%%--------------------------------------------------------------------
prefix_search(ParentDir, Marker, MaxKeys) ->
    ?debug("prefix_search/3", "Parent Dir: ~p, Marker: ~p", [ParentDir, Marker]),
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
-spec(prefix_search_and_remove_objects(MQId, ParentDir, EnqueuedAt) ->
             {ok, [{Key, Value}]} |
             not_found |
             {error, Cause} when MQId::mq_id(),
                                 ParentDir::binary(),
                                 EnqueuedAt::pos_integer(),
                                 Key::atom(),
                                 Value::any(),
                                 Cause::any()).
prefix_search_and_remove_objects(MQId, ParentDir, EnqueuedAt) ->
    Fun = fun(Key, V, Acc) ->
                  PreMeta = binary_to_term(V),
                  case leo_object_storage_transformer:transform_metadata(PreMeta) of
                      {error, Cause} ->
                          ?error("prefix_search_and_remove_objects/3",
                                 [{key, Key}, {error, Cause}]),
                          Acc;
                      Metadata ->
                          AddrId = Metadata#?METADATA.addr_id,
                          Pos_1 = case binary:match(Key, [ParentDir]) of
                                      nomatch ->
                                          -1;
                                      {Pos,_} ->
                                          Pos
                                  end,
                          case (Pos_1 == 0) of
                              true when Metadata#?METADATA.del == ?DEL_FALSE ->
                                  case leo_storage_mq:publish(
                                         {?QUEUE_ID_DEL_DIR, MQId}, AddrId, Key, EnqueuedAt) of
                                      ok ->
                                          void;
                                      {error, Cause} ->
                                          ?warn("prefix_search_and_remove_objects/1",
                                                [{qid, MQId}, {addr_id, AddrId},
                                                 {key, Key}, {cause, Cause}])
                                  end,
                                  Acc;
                              _ ->
                                  Acc
                          end
                  end
          end,
    case catch leo_object_storage_api:fetch_by_key_in_parallel(ParentDir, Fun, undefined) of
        {'EXIT', Cause} ->
            {error, Cause};
        {ok,_} ->
            {ok, MsgCount} = leo_mq_api:count(MQId),
            {ok, MsgCount};
        not_found ->
            {ok, 0};
        Error ->
            Error
    end.


%% @doc Find already uploaded objects by original-filename
-spec(find_uploaded_objects_by_key(OriginalKey) ->
             {ok, [Metadata]} | not_found when OriginalKey::binary(),
                                               Metadata::#?METADATA{}).
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


%% @doc Investigate the key whether it is under any deletion-directory or not
-spec(is_key_under_del_dir(Key) ->
             Result when Key::binary(),
                         Result::boolean()).
is_key_under_del_dir(Key) ->
    case leo_storage_handler_del_directory:get_cached_items() of
        {ok, Dirs} ->
            is_key_under_del_dir(Dirs, Key);
        {error, Cause} ->
            ?error("is_key_under_del_dir/1", [{cause, Cause}]),
            false
    end.

is_key_under_del_dir([],_) ->
    false;
is_key_under_del_dir([Dir|Acc], Key) ->
    Dir_1 = case catch binary:part(Dir, (byte_size(Dir) - 1), 1) of
                <<"/">> ->
                    Dir;
                _ ->
                    << Dir/binary, "/" >>
            end,
    case binary:match(Key, [Dir_1],[]) of
        {0,_} ->
            true;
        _ ->
            is_key_under_del_dir(Acc, Key)
    end.


%% @doc Investigate the object whether it can do data-compaction
-spec(can_compact_object(Key, NumOfReplicas) ->
             boolean() when Key::binary(),
                            NumOfReplicas::non_neg_integer()).
can_compact_object(Key, NumOfReplicas) ->
    case is_key_under_del_dir(Key) of
        true ->
            false;
        false ->
            leo_redundant_manager_api:has_charge_of_node(Key, NumOfReplicas)
    end.


%%--------------------------------------------------------------------
%% INNNER FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Retrieve active redundancies
%% @private
-spec(get_active_redundancies(Quorum, Redundancies) ->
             {ok, Redundancies} |
             {error, Cause} when Quorum::non_neg_integer(),
                                 Redundancies::[#redundant_node{}],
                                 Cause::any()).
get_active_redundancies(_, []) ->
    {error, ?ERROR_NOT_SATISFY_QUORUM};
get_active_redundancies(Quorum, Redundancies) ->
    AvailableNodes = [RedundantNode ||
                         #redundant_node{available = true} = RedundantNode <- Redundancies],
    HasUnavailableNodes = lists:any(fun(#redundant_node{available = false}) ->
                                            true;
                                       (_) ->
                                            false
                                    end, Redundancies),
    case (Quorum =< erlang:length(AvailableNodes)) of
        true ->
            {ok, AvailableNodes, HasUnavailableNodes};
        false ->
            {error, ?ERROR_NOT_SATISFY_QUORUM}
    end.


%% @doc read reapir - compare with remote-node's meta-data.
%% @private
-spec(read_and_repair(ReadParams, Redundancies) ->
             {ok, Metadata, Bin} |
             {ok, match} |
             {error, Cause} when ReadParams::#read_parameter{},
                                 Redundancies::[#redundant_node{}],
                                 Metadata::#?METADATA{},
                                 Bin::binary(),
                                 Cause::any()).
read_and_repair(#read_parameter{quorum = Q} = ReadParams, Redundancies) ->
    case get_active_redundancies(Q, Redundancies) of
        {ok, AvailableNodes, HasUnavailableNodes} ->
            read_and_repair_1(ReadParams, AvailableNodes, AvailableNodes, HasUnavailableNodes, []);
        Error ->
            Error
    end.

%% @private
read_and_repair_1(_,[],_, false, Errors) ->
    case lists:any(fun(not_found) ->
                           false;
                      (_) ->
                           true
                   end, Errors) of
        false ->
            {error, not_found};
        _ ->
            %% If some nodes are missing, reply the state is uncertain
            {error, ?ERROR_RECOVER_FAILURE}
    end;
read_and_repair_1(_,[],_,_,_) ->
    {error, ?ERROR_RECOVER_FAILURE};
read_and_repair_1(ReadParams, [Node|Rest], AvailableNodes, HasUnavailableNodes, Errors) ->
    case read_and_repair_2(ReadParams, Node, AvailableNodes) of
        {error, Cause} ->
            read_and_repair_1(ReadParams, Rest, AvailableNodes, HasUnavailableNodes, [Cause|Errors]);
        Ret ->
            Ret
    end.

%% @private
-spec(read_and_repair_2(ReadParams, Redundancies, Redundancies) ->
             {ok, Metadata, Bin} |
             {ok, match} |
             {error, Cause} when ReadParams::#read_parameter{},
                                 Redundancies::[atom()],
                                 Metadata::#?METADATA{},
                                 Bin::binary(),
                                 Cause::any()).
read_and_repair_2(_, [], _) ->
    {error, not_found};

%% Request retrieving an object to 'LOCAL' (etag == 0)
read_and_repair_2(#read_parameter{addr_id = AddrId,
                                  key = Key,
                                  etag = 0,
                                  start_pos = StartPos,
                                  end_pos = EndPos} = ReadParameter,
                  #redundant_node{node = Node}, Redundancies) when Node == erlang:node() ->
    LeftRedundancies = [RedundantNode ||
                           #redundant_node{node = RNode} = RedundantNode <- Redundancies, RNode =/= Node],
    read_and_repair_3(
      get_fun(AddrId, Key, StartPos, EndPos), ReadParameter, LeftRedundancies);

%% Request retrieving an object to 'LOCAL' (etag /= 0)
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
            LeftRedundancies = [RedundantNode ||
                                   #redundant_node{node = RNode} = RedundantNode <- Redundancies, RNode =/= Node],
            read_and_repair_3(
              get_fun(AddrId, Key, StartPos, EndPos), ReadParameter, LeftRedundancies)
    end;

%% Request retrieving an object to 'REMOTE'
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
    LeftRedundancies = [RedundantNode ||
                           #redundant_node{node = RNode} = RedundantNode <- Redundancies, RNode =/= Node],
    read_and_repair_3(RetRPC, ReadParameter, LeftRedundancies).

%% @private
read_and_repair_3({ok, Metadata, #?OBJECT{data = Bin}}, #read_parameter{}, []) ->
    {ok, Metadata, Bin};
read_and_repair_3({ok, match} = Reply, #read_parameter{},_Redundancies) ->
    Reply;

read_and_repair_3({ok, Metadata, #?OBJECT{data = Bin,
                                          num_of_replicas = NumOfReplicas,
                                          preferred_r = Preferred_R}},
                  #read_parameter{quorum = Quorum} = ReadParameter, Redundancies) ->
    %% If 'object' contains 'num_of_replicas > 0' and 'preferred_r > 0',
    %% those params are adopted instead of the parametes of the local consistency level.
    {Quorum_1, Redundancies_1} =
        case (NumOfReplicas > 0 andalso
              Preferred_R > 0) of
            true ->
                {Preferred_R, lists:sublist(Redundancies, NumOfReplicas - 1)};
            false ->
                {Quorum, Redundancies}
        end,

    Fun = fun(ok) ->
                  {ok, Metadata, Bin};
             ({error, unavailable} = Ret) ->
                  Ret;
             ({error,_Cause}) ->
                  {error, ?ERROR_RECOVER_FAILURE}
          end,
    ReadParameter_1 = ReadParameter#read_parameter{quorum = Quorum_1},
    leo_storage_read_repairer:repair(ReadParameter_1, Redundancies_1, Metadata, Fun);

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
-spec(replicate_fun(ReplicationMethod, Method, AddrId, Object, From) ->
             {ok, ETag} |
             {error, Cause} when ReplicationMethod::replication(),
                                 Method::request_verb(),
                                 AddrId::integer(),
                                 Object::#?OBJECT{},
                                 From::atom(),
                                 ETag::{etag, integer()},
                                 Cause::any()).
replicate_fun(?REP_LOCAL, Method, AddrId, Object, From) ->
    %% Check state of the node
    case leo_watchdog_state:find_not_safe_items(?WD_EXCLUDE_ITEMS) of
        not_found ->
            case leo_redundant_manager_api:get_redundancies_by_addr_id(put, AddrId) of
                {ok, #redundancies{nodes = Redundancies,
                                   n = NumOfReplicas,
                                   w = WriteQuorum,
                                   d = DeleteQuorum,
                                   ring_hash = RingHash}} ->
                    Quorum = case From of
                                 leo_mq ->
                                     ?quorum(Method, NumOfReplicas, NumOfReplicas);
                                 _ ->
                                     ?quorum(Method, WriteQuorum, DeleteQuorum)
                             end,
                    leo_storage_replicator:replicate(
                      Method, Quorum, Redundancies,
                      Object#?OBJECT{ring_hash = RingHash},
                      replicate_callback(Object));
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

-spec(replicate_callback(Object|null) ->
             Fun when Object::#?OBJECT{},
                      Fun::function()).
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

-ifdef(EUNIT).
get_cmeta_test() ->
    ok = leo_logger_api:new("./", ?LOG_LEVEL_WARN),
    %% destination side with custom metadata
    UDM = [{<<"name">>, <<"LeoFS">>},
           {<<"category">>, <<"distributed storage">>},
           {<<"url">>, <<"leo-project.net/leofs">>}
          ],
    CMeta = [{?PROP_CMETA_CLUSTER_ID, 'leofs_1'},
             {?PROP_CMETA_NUM_OF_REPLICAS, 3},
             {?PROP_CMETA_VER, leo_date:clock()},
             {?PROP_CMETA_UDM, UDM}
            ],
    Meta1 = #?METADATA{meta = term_to_binary(CMeta)},
    {ok, Meta11} = get_cmeta(Meta1),
    ?assertEqual(term_to_binary(UDM), Meta11#?METADATA.meta),

    %% soruce side with custom metadata
    Meta2 = #?METADATA{meta = term_to_binary(UDM)},
    {ok, Meta2} = get_cmeta(Meta2),

    %% soruce side with broken custom metadata
    Meta3 = #?METADATA{meta = <<"broken">>},
    {ok, #?METADATA{meta = <<>>} = _Meta} = get_cmeta(Meta3),

    %% destination side with broken custom metadata
    %% tuple nested doubly
    CMeta2 = [{?PROP_CMETA_CLUSTER_ID, 'leofs_1'},
              {?PROP_CMETA_NUM_OF_REPLICAS, 3},
              {?PROP_CMETA_VER, leo_date:clock()},
              {?PROP_CMETA_UDM, {ok, UDM}}
             ],
    Meta4 = #?METADATA{meta = term_to_binary(CMeta2)},
    {ok, Meta41} = get_cmeta(Meta4),
    ?assertEqual(term_to_binary(UDM), Meta41#?METADATA.meta),

    %% destination side with broken custom metadata
    %% tuple nested triply
    CMeta3 = [{?PROP_CMETA_CLUSTER_ID, 'leofs_1'},
              {?PROP_CMETA_NUM_OF_REPLICAS, 3},
              {?PROP_CMETA_VER, leo_date:clock()},
              {?PROP_CMETA_UDM, {ok, {ok, UDM}}}
             ],
    Meta5 = #?METADATA{meta = term_to_binary(CMeta3)},
    {ok, Meta51} = get_cmeta(Meta5),
    ?assertEqual(term_to_binary(UDM), Meta51#?METADATA.meta),

    ok.
-endif.

%% @doc PUT an object having inconsistencies for test/debug
debug_put(Key, Body, NumOfReplicas) ->
    case leo_redundant_manager_api:get_redundancies_by_key(put, Key) of
        {ok, #redundancies{nodes = Redundancies,
                              id = AddrId,
                               n = NumOfReplicasOrg,
                               ring_hash = RingHash}} ->
            NumOfReplicas2 = case NumOfReplicas > NumOfReplicasOrg of
                                 true ->
                                     NumOfReplicasOrg;
                                 false ->
                                     NumOfReplicas
                             end,
            Redundancies2 = lists:sublist(Redundancies, NumOfReplicas2),
            Quorum = ?quorum(?CMD_PUT, NumOfReplicas2, NumOfReplicas2),
            Object = #?OBJECT{method = ?CMD_PUT,
                             addr_id = AddrId,
                             key = Key,
                             data = Body,
                             dsize = size(Body),
                             timestamp = leo_date:now(),
                             clock = leo_date:clock(),
                             ring_hash = RingHash},
            leo_storage_replicator:replicate(
                ?CMD_PUT, Quorum, Redundancies2,
                Object,
                replicate_callback(Object));
        {error, Cause} ->
            {error, Cause}
    end.

%% @doc DELETE an object having inconsistencies for test/debug
debug_delete(Key, NumOfReplicas) ->
    case leo_redundant_manager_api:get_redundancies_by_key(delete, Key) of
        {ok, #redundancies{nodes = Redundancies,
                              id = AddrId,
                               n = NumOfReplicasOrg,
                               ring_hash = RingHash}} ->
            NumOfReplicas2 = case NumOfReplicas > NumOfReplicasOrg of
                                 true ->
                                     NumOfReplicasOrg;
                                 false ->
                                     NumOfReplicas
                             end,
            Redundancies2 = lists:sublist(Redundancies, NumOfReplicas2),
            Quorum = ?quorum(?CMD_DELETE, NumOfReplicas2, NumOfReplicas2),
            Object = #?OBJECT{method = ?CMD_DELETE,
                             addr_id = AddrId,
                             key = Key,
                             data = <<>>,
                             dsize = 0,
                             timestamp = leo_date:now(),
                             clock = leo_date:clock(),
                             ring_hash = RingHash,
                             del = ?DEL_TRUE},
            leo_storage_replicator:replicate(
                ?CMD_DELETE, Quorum, Redundancies2,
                Object,
                replicate_callback(Object));
        {error, Cause} ->
            {error, Cause}
    end.
