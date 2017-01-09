%%====================================================================
%%
%% LeoFS Gateway
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
%% -------------------------------------------------------------------
%% LeoFS Gateway - RPC Handler Test
%% @doc
%% @end
%%====================================================================
-module(leo_gateway_rpc_handler_tests).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_s3_libs/include/leo_s3_auth.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").


%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
api_test_() ->
    {setup, fun setup/0, fun teardown/1,
     {with, [
             fun head_object_error_/1,
             fun head_object_notfound_/1,
             fun head_object_normal1_/1,
             fun get_object_error_/1,
             fun get_object_notfound_/1,
             fun get_object_normal1_/1,
             fun get_object_with_etag_error_/1,
             fun get_object_with_etag_notfound_/1,
             fun get_object_with_etag_normal1_/1,
             fun get_object_with_etag_normal2_/1,
             fun delete_object_error_/1,
             fun delete_object_notfound_/1,
             fun delete_object_normal1_/1,
             fun put_object_error_/1,
             fun put_object_notfound_/1,
             fun put_object_normal1_/1
            ]}}.

setup() ->
    ok = leo_logger_client_message:new("./", ?LOG_LEVEL_WARN),
    io:format(user, "cwd:~p~n",[os:cmd("pwd")]),
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),
    Node0 = list_to_atom("node_0@" ++ Hostname),
    net_kernel:start([Node0, shortnames]),

    Args = " -pa ../deps/*/ebin "
        ++ " -kernel error_logger    '{file, \"../kernel.log\"}' "
        ++ " -sasl sasl_error_logger '{file, \"../sasl.log\"}' ",
    {ok, Node1} = slave:start_link(list_to_atom(Hostname), 'manager_0', Args),

    ok = leo_misc:init_env(),

    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(_Method, _Key) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0,
                                                                    available = true},
                                                    #redundant_node{node = Node1,
                                                                    available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),
    ok = rpc:call(Node1, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),

    leo_pod:start_link(?POD_LOH_WORKER,
                       ?env_loh_put_worker_pool_size(),
                       ?env_loh_put_worker_buffer_size(),
                       leo_large_object_worker, [],
                       fun(_) ->
                               void
                       end),

    [Node0, Node1].

teardown([_, Node1]) ->
    meck:unload(),
    net_kernel:stop(),
    slave:stop(Node1),
    ok.

head_object_notfound_([Node0, Node1]) ->
    ok = rpc:call(Node0, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, head,
                                        fun(_Addr, _Key) ->
                                                {error, not_found}
                                        end]),
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, head,
                                        fun(_Addr, _Key) ->
                                                {error, not_found}
                                        end]),
    Res = leo_gateway_rpc_handler:head(<<"bucket/key">>),
    ?assertEqual({error, not_found}, Res),
    ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

head_object_error_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, head,
                                        fun(_Addr, _Key) ->
                                                {error, internal_server_error}
                                        end]),
    Res = leo_gateway_rpc_handler:head(<<"bucket/key">>),
    ?assertEqual({error, internal_server_error}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

head_object_normal1_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, head,
                                        fun(_Addr, Key) ->
                                                {ok,
                                                 #?METADATA{
                                                     key = Key,
                                                     dsize = 10,
                                                     del = 0
                                                    }
                                                }
                                        end]),
    KEY = <<"bucket/key">>,
    {ok, Meta} = leo_gateway_rpc_handler:head(KEY),
    ?assertEqual(10, Meta#?METADATA.dsize),
    ?assertEqual(KEY, Meta#?METADATA.key),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_notfound_([Node0, Node1]) ->
    ok = rpc:call(Node0, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _ReqId) ->
                                                {error, not_found}
                                        end]),
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _ReqId) ->
                                                {error, not_found}
                                        end]),
    Res = leo_gateway_rpc_handler:get(<<"bucket/key">>),
    ?assertEqual({error, not_found}, Res),
    ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_error_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _ReqId) ->
                                                {error, internal_server_error}
                                        end]),
    Res = leo_gateway_rpc_handler:get(<<"bucket/key">>),
    ?assertEqual({error, internal_server_error}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_normal1_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    CMetaBin = term_to_binary([{<<"x-amz-meta-test">>, <<"cmeta">>}]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, Key, _ReqId) ->
                                                {ok,
                                                 #?METADATA{
                                                     key = Key,
                                                     meta = CMetaBin,
                                                     msize = byte_size(CMetaBin),
                                                     dsize = 4,
                                                     del = 0
                                                    },
                                                 <<"body">>
                                                }
                                        end]),
    KEY = <<"bucket/key">>,
    {ok, Meta, Body} = leo_gateway_rpc_handler:get(KEY),
    ?assertEqual(4, Meta#?METADATA.dsize),
    ?assertEqual(KEY, Meta#?METADATA.key),
    ?assertEqual(<<"body">>, Body),
    ?assertEqual(term_to_binary([{<<"x-amz-meta-test">>, <<"cmeta">>}]), Meta#?METADATA.meta),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_with_etag_notfound_([Node0, Node1]) ->
    ok = rpc:call(Node0, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _Etag, _ReqId) ->
                                                {error, not_found}
                                        end]),
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _Etag, _ReqId) ->
                                                {error, not_found}
                                        end]),
    Res = leo_gateway_rpc_handler:get(<<"bucket/key">>, 123),
    ?assertEqual({error, not_found}, Res),
    ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_with_etag_error_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _Etag, _ReqId) ->
                                                {error, internal_server_error}
                                        end]),
    Res = leo_gateway_rpc_handler:get(<<"bucket/key">>, 123),
    ?assertEqual({error, internal_server_error}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_with_etag_normal1_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    CMetaBin = term_to_binary([{<<"x-amz-meta-test">>, <<"cmeta">>}]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, Key, _Etag, _ReqId) ->
                                                {ok,
                                                 #?METADATA{
                                                     key = Key,
                                                     meta = CMetaBin,
                                                     msize = byte_size(CMetaBin),
                                                     dsize = 4,
                                                     del = 0
                                                    },
                                                 <<"body">>
                                                }
                                        end]),
    KEY = <<"bucket/key">>,
    {ok, Meta, Body} = leo_gateway_rpc_handler:get(KEY, 123),
    ?assertEqual(4, Meta#?METADATA.dsize),
    ?assertEqual(KEY, Meta#?METADATA.key),
    ?assertEqual(<<"body">>, Body),
    ?assertEqual(term_to_binary([{<<"x-amz-meta-test">>, <<"cmeta">>}]), Meta#?METADATA.meta),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

get_object_with_etag_normal2_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, get,
                                        fun(_Addr, _Key, _Etag, _ReqId) ->
                                                {ok, match}
                                        end]),
    Res = leo_gateway_rpc_handler:get(<<"bucket/key">>, 123),
    ?assertEqual({ok, match}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

delete_object_notfound_([Node0, Node1]) ->
    ok = rpc:call(Node0, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, delete,
                                        fun(_, _) ->
                                                {error, not_found}
                                        end]),
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, delete,
                                        fun(_, _) ->
                                                {error, not_found}
                                        end]),
    Res = leo_gateway_rpc_handler:delete(<<"bucket/key">>),
    ?assertEqual({error, not_found}, Res),
    ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

delete_object_error_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, delete,
                                        fun(_, _) ->
                                                {error, internal_server_error}
                                        end]),
    Res = leo_gateway_rpc_handler:delete(<<"bucket/key">>),
    ?assertEqual({error, internal_server_error}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

delete_object_normal1_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, delete,
                                        fun(_, _) ->
                                                ok
                                        end]),
    Res = leo_gateway_rpc_handler:delete(<<"bucket/key">>),
    ?assertEqual(ok, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

put_object_notfound_([Node0, Node1]) ->
    ok = rpc:call(Node0, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, put,
                                        fun(_, _) ->
                                                {error, not_found}
                                        end]),
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put,
                                        fun(_, _) ->
                                                {error, not_found}
                                        end]),
    Res = leo_gateway_rpc_handler:put(#put_req_params{path = <<"bucket/key">>,
                                                      body =  <<"body">>,
                                                      dsize = 4}),
    ?assertEqual({error, not_found}, Res),
    ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

put_object_error_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put,
                                        fun(_,_) ->
                                                {error, internal_server_error}
                                        end]),
    Res = leo_gateway_rpc_handler:put(#put_req_params{path = <<"bucket/key">>,
                                                      body =  <<"body">>,
                                                      dsize = 4}),
    ?assertEqual({error, internal_server_error}, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.

put_object_normal1_([_Node0, Node1]) ->
    ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put,
                                        fun(_, _) ->
                                                ok
                                        end]),
    Res = leo_gateway_rpc_handler:put(#put_req_params{path = <<"bucket/key">>,
                                                      body =  <<"body">>,
                                                      dsize = 4}),
    ?assertEqual(ok, Res),
    ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
    ok.
-endif.
