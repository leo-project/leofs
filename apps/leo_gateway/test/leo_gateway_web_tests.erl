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
%% LeoFS Gateway - S3 domainogics Test
%% @doc
%% @end
%%====================================================================
-module(leo_gateway_web_tests).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TARGET_HOST, "localhost").

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
s3_api_test_() ->
    {setup,
     fun setup_s3_api/0,
     fun teardown/1,
     fun gen_tests_1/1}.

rest_api_test_() ->
    {setup,
     fun setup_rest_api/0,
     fun teardown/1,
     fun gen_tests_2/1}.

gen_tests_1(Arg) ->
    lists:map(fun(Test) -> Test(Arg) end,
              [fun get_bucket_list_error_/1,
               fun get_bucket_list_empty_/1,
               fun get_bucket_list_normal1_/1,
               fun get_bucket_acl_normal1_/1,
               fun head_object_error_/1,
               fun head_object_notfound_/1,
               fun head_object_normal1_/1,
               fun get_object_error_/1,
               fun get_object_invalid_/1,
               fun get_object_notfound_/1,
               fun get_object_normal1_/1,
               fun get_object_cmeta_normal1_/1,
               fun range_object_normal1_/1,
               fun range_object_normal2_/1,
               fun range_object_normal3_/1,
               fun delete_object_error_/1,
               fun delete_object_notfound_/1,
               fun delete_object_normal1_/1,
               fun put_object_error_/1,
               fun put_object_error_metadata_too_large_/1,
               fun put_object_normal1_/1,
               fun put_object_aws_chunked_/1,
               fun put_object_aws_chunked_error_/1
              ]).

gen_tests_2(Arg) ->
    lists:map(fun(Test) -> Test(Arg) end,
              [fun head_object_error_/1,
               fun head_object_notfound_/1,
               fun head_object_normal1_/1,
               fun get_object_error_/1,
               fun get_object_notfound_/1,
               fun get_object_normal1_/1,
               fun delete_object_error_/1,
               fun delete_object_notfound_/1,
               fun delete_object_normal1_/1,
               fun put_object_error_/1,
               fun put_object_normal1_/1
              ]).

-define(SSL_CERT_DATA,
        "-----BEGIN CERTIFICATE-----\n" ++
            "MIIDIDCCAgigAwIBAgIJAJLkNZzERPIUMA0GCSqGSIb3DQEBBQUAMBQxEjAQBgNV\n" ++
            "BAMTCWxvY2FsaG9zdDAeFw0xMDAzMTgxOTM5MThaFw0yMDAzMTUxOTM5MThaMBQx\n" ++
            "EjAQBgNVBAMTCWxvY2FsaG9zdDCCASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoC\n" ++
            "ggEBAJeUCOZxbmtngF4S5lXckjSDLc+8C+XjMBYBPyy5eKdJY20AQ1s9/hhp3ulI\n" ++
            "8pAvl+xVo4wQ+iBSvOzcy248Q+Xi6+zjceF7UNRgoYPgtJjKhdwcHV3mvFFrS/fp\n" ++
            "9ggoAChaJQWDO1OCfUgTWXImhkw+vcDR11OVMAJ/h73dqzJPI9mfq44PTTHfYtgr\n" ++
            "v4LAQAOlhXIAa2B+a6PlF6sqDqJaW5jLTcERjsBwnRhUGi7JevQzkejujX/vdA+N\n" ++
            "jRBjKH/KLU5h3Q7wUchvIez0PXWVTCnZjpA9aR4m7YV05nKQfxtGd71czYDYk+j8\n" ++
            "hd005jetT4ir7JkAWValBybJVksCAwEAAaN1MHMwHQYDVR0OBBYEFJl9s51SnjJt\n" ++
            "V/wgKWqV5Q6jnv1ZMEQGA1UdIwQ9MDuAFJl9s51SnjJtV/wgKWqV5Q6jnv1ZoRik\n" ++
            "FjAUMRIwEAYDVQQDEwlsb2NhbGhvc3SCCQCS5DWcxETyFDAMBgNVHRMEBTADAQH/\n" ++
            "MA0GCSqGSIb3DQEBBQUAA4IBAQB2ldLeLCc+lxK5i0EZquLamMBJwDIjGpT0JMP9\n" ++
            "b4XQOK2JABIu54BQIZhwcjk3FDJz/uOW5vm8k1kYni8FCjNZAaRZzCUfiUYTbTKL\n" ++
            "Rq9LuIAODyP2dnTqyKaQOOJHvrx9MRZ3XVecXPS0Tib4aO57vCaAbIkmhtYpTWmw\n" ++
            "e3t8CAIDVtgvjR6Se0a1JA4LktR7hBu22tDImvCSJn1nVAaHpani6iPBPPdMuMsP\n" ++
            "TBoeQfj8VpqBUjCStqJGa8ytjDFX73YaxV2mgrtGwPNme1x3YNRR11yTu7tksyMO\n" ++
            "GrmgxNriqYRchBhNEf72AKF0LR1ByKwfbDB9rIsV00HtCgOp\n" ++
            "-----END CERTIFICATE-----\n").
-define(SSL_KEY_DATA,
        "-----BEGIN RSA PRIVATE KEY-----\n" ++
            "MIIEpAIBAAKCAQEAl5QI5nFua2eAXhLmVdySNIMtz7wL5eMwFgE/LLl4p0ljbQBD\n" ++
            "Wz3+GGne6UjykC+X7FWjjBD6IFK87NzLbjxD5eLr7ONx4XtQ1GChg+C0mMqF3Bwd\n" ++
            "Xea8UWtL9+n2CCgAKFolBYM7U4J9SBNZciaGTD69wNHXU5UwAn+Hvd2rMk8j2Z+r\n" ++
            "jg9NMd9i2Cu/gsBAA6WFcgBrYH5ro+UXqyoOolpbmMtNwRGOwHCdGFQaLsl69DOR\n" ++
            "6O6Nf+90D42NEGMof8otTmHdDvBRyG8h7PQ9dZVMKdmOkD1pHibthXTmcpB/G0Z3\n" ++
            "vVzNgNiT6PyF3TTmN61PiKvsmQBZVqUHJslWSwIDAQABAoIBACI8Ky5xHDFh9RpK\n" ++
            "Rn/KC7OUlTpADKflgizWJ0Cgu2F9L9mkn5HyFHvLHa+u7CootbWJOiEejH/UcBtH\n" ++
            "WyMQtX0snYCpdkUpJv5wvMoebGu+AjHOn8tfm9T/2O6rhwgckLyMb6QpGbMo28b1\n" ++
            "p9QiY17BJPZx7qJQJcHKsAvwDwSThlb7MFmWf42LYWlzybpeYQvwpd+UY4I0WXLu\n" ++
            "/dqJIS9Npq+5Y5vbo2kAEAssb2hSCvhCfHmwFdKmBzlvgOn4qxgZ1iHQgfKI6Z3Y\n" ++
            "J0573ZgOVTuacn+lewtdg5AaHFcl/zIYEr9SNqRoPNGbPliuv6k6N2EYcufWL5lR\n" ++
            "sCmmmHECgYEAxm+7OpepGr++K3+O1e1MUhD7vSPkKJrCzNtUxbOi2NWj3FFUSPRU\n" ++
            "adWhuxvUnZgTcgM1+KuQ0fB2VmxXe9IDcrSFS7PKFGtd2kMs/5mBw4UgDZkOQh+q\n" ++
            "kDiBEV3HYYJWRq0w3NQ/9Iy1jxxdENHtGmG9aqamHxNtuO608wGW2S8CgYEAw4yG\n" ++
            "ZyAic0Q/U9V2OHI0MLxLCzuQz17C2wRT1+hBywNZuil5YeTuIt2I46jro6mJmWI2\n" ++
            "fH4S/geSZzg2RNOIZ28+aK79ab2jWBmMnvFCvaru+odAuser4N9pfAlHZvY0pT+S\n" ++
            "1zYX3f44ygiio+oosabLC5nWI0zB2gG8pwaJlaUCgYEAgr7poRB+ZlaCCY0RYtjo\n" ++
            "mYYBKD02vp5BzdKSB3V1zeLuBWM84pjB6b3Nw0fyDig+X7fH3uHEGN+USRs3hSj6\n" ++
            "BqD01s1OT6fyfbYXNw5A1r+nP+5h26Wbr0zblcKxdQj4qbbBZC8hOJNhqTqqA0Qe\n" ++
            "MmzF7jiBaiZV/Cyj4x1f9BcCgYEAhjL6SeuTuOctTqs/5pz5lDikh6DpUGcH8qaV\n" ++
            "o6aRAHHcMhYkZzpk8yh1uUdD7516APmVyvn6rrsjjhLVq4ZAJjwB6HWvE9JBN0TR\n" ++
            "bILF+sREHUqU8Zn2Ku0nxyfXCKIOnxlx/J/y4TaGYqBqfXNFWiXNUrjQbIlQv/xR\n" ++
            "K48g/MECgYBZdQlYbMSDmfPCC5cxkdjrkmAl0EgV051PWAi4wR+hLxIMRjHBvAk7\n" ++
            "IweobkFvT4TICulgroLkYcSa5eOZGxB/DHqcQCbWj3reFV0VpzmTDoFKG54sqBRl\n" ++
            "vVntGt0pfA40fF17VoS7riAdHF53ippTtsovHEsg5tq5NrBl5uKm2g==\n" ++
            "-----END RSA PRIVATE KEY-----\n").

setup(InitFun, TermFun) ->
    ok = leo_logger_client_message:new("./", ?LOG_LEVEL_WARN),
    ok = leo_logger_client_base:new(?LOG_GROUP_ID_ACCESS, ?LOG_ID_ACCESS,
                                    "./", ?LOG_FILENAME_ACCESS),

    io:format(user, "cwd:~p~n",[os:cmd("pwd")]),
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),
    NetKernelNode = list_to_atom("netkernel_0@" ++ Hostname),
    net_kernel:start([NetKernelNode, shortnames]),
    inets:start(),

    Args = " -pa ../deps/*/ebin "
        ++ " -kernel error_logger    '{file, \"../kernel.log\"}' ",
    {ok, Node0} = slave:start_link(list_to_atom(Hostname), 'storage_0', Args),
    {ok, Node1} = slave:start_link(list_to_atom(Hostname), 'storage_1', Args),

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
    meck:new(leo_s3_endpoint, [non_strict]),
    meck:expect(leo_s3_endpoint, get_endpoints, 0, {ok, [{endpoint, <<"localhost">>, 0}]}),

    meck:new(leo_s3_bucket, [non_strict]),
    meck:expect(leo_s3_bucket, get_latest_bucket,
                fun(_BucketName) ->
                        {ok, #?BUCKET{name =_BucketName,
                                      acls = [#bucket_acl_info{user_id = ?GRANTEE_ALL_USER,
                                                               permissions = [read, write]}]}}
                end),

    Date = erlang:list_to_binary(leo_http:rfc1123_date(leo_date:now())),
    meck:new(cowboy_clock, [non_strict]),
    meck:expect(cowboy_clock, rfc1123, 0, Date),

    meck:new(leo_watchdog_state, [non_strict]),
    meck:expect(leo_watchdog_state, find_not_safe_items, fun() -> not_found end),

    meck:new(leo_metrics_req, [non_strict]),
    meck:expect(leo_metrics_req, notify, fun(_) -> ok end),
    ok = rpc:call(Node1, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),

    code:add_path("../cherly/ebin"),
    ok = file:write_file("./server_cert.pem", ?SSL_CERT_DATA),
    ok = file:write_file("./server_key.pem",  ?SSL_KEY_DATA),

    application:start(leo_cache),
    leo_cache_api:start(),

    leo_pod:start_link(?POD_LOH_WORKER,
                       ?env_loh_put_worker_pool_size(),
                       ?env_loh_put_worker_buffer_size(),
                       leo_large_object_worker, [],
                       fun(_) ->
                               void
                       end),

    InitFun(),
    [TermFun, Node0, Node1].

setup_s3_api() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),

    {ok, Options} = leo_gateway_app:get_options(),
    InitFun = fun() -> leo_gateway_http_commons:start(
                         Options#http_options{port = 8080}) end,
    TermFun = fun() -> leo_gateway_s3_api:stop() end,
    setup(InitFun, TermFun).

setup_rest_api() ->
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),

    {ok, Options} = leo_gateway_app:get_options(),
    InitFun = fun() -> leo_gateway_http_commons:start(
                         Options#http_options{handler = leo_gateway_rest_api,
                                              port = 8080}) end,
    TermFun = fun() -> leo_gateway_rest_api:stop() end,
    setup(InitFun, TermFun).

teardown([TermFun, Node0, Node1]) ->
    inets:stop(),
    net_kernel:stop(),
    slave:stop(Node0),
    slave:stop(Node1),

    meck:unload(),
    TermFun(),

    cowboy:stop_listener(leo_gateway_s3_api),
    cowboy:stop_listener(leo_gateway_s3_api_ssl),

    application:stop(crypto),
    application:stop(ranch),
    application:stop(cowboy),
    leo_cache_api:stop(),
    timer:sleep(250),
    ok.

get_bucket_list_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            timer:sleep(150),

            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_directory, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_directory,
                           find_by_parent_dir, 1, {error, some_error}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(get, {lists:append(
                                          ["http://", ?TARGET_HOST, ":8080/a/b?prefix=pre"]), [{"Date", Date}]},
                                  [], [{full_result, false}]),

                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_InternalError,
                                     ?XML_ERROR_MSG_InternalError, "a/b/", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(500, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_directory])
            end,
            ok
    end.

get_bucket_list_empty_([_TermFun, _Node0, Node1]) ->
    fun() ->
            timer:sleep(150),
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_directory, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_directory, find_by_parent_dir, 4, {ok, []}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b?prefix=pre&delimiter=/"]),
                                                      [{"Date", Date}]},
                                  [], [{full_result, false}]),
                ?assertEqual(200, SC),

                Xml = io_lib:format(?XML_OBJ_LIST, ["a", "pre", "1000", "", "false" "", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body))
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_directory])
            end,
            ok
    end.

get_bucket_list_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            timer:sleep(150),
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_directory, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_directory, find_by_parent_dir,
                           4, {ok,
                               [#?METADATA{
                                    key = <<"localhost/a/b/pre/test.png">>,
                                    addr_id    = 0,
                                    ksize      = 8,
                                    dsize      = 0,
                                    meta       = <<>>,
                                    msize      = 0,
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 0,
                                    clock      = 63511805822,
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   }
                                %% {metadata, <<"localhost/a/b/pre/test.png">>,
                                %%  0, 8, 0, 0,
                                %%  0, 0, 0,
                                %%  0, 0, 63511805822, 19740926, 0, 0}
                               ]}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC,Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST, ":8080/a/b/?prefix=pre&delimiter=/"]), [{"Date", Date}]},
                                  [], [{full_result, false}]),
                ?assertEqual(200, SC),
                {_XmlDoc, Rest} = xmerl_scan:string(Body),
                ?assertEqual([], Rest)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_directory])
            end,
            ok
    end.

get_bucket_acl_normal1_([_TermFun, _Node0,_Node1]) ->
    fun() ->
            timer:sleep(150),
            %% leo_s3_bucket is already created at setup
            meck:expect(leo_s3_bucket, get_acls, 1,
                        {ok, [#bucket_acl_info{user_id = ?GRANTEE_ALL_USER,
                                               permissions = [read, write]}]}),
            meck:expect(leo_s3_bucket, find_bucket_by_name, 1,
                        {ok, #?BUCKET{name = "bucket",
                                      access_key_id = <<"ackid">>,
                                      acls = [#bucket_acl_info{user_id = ?GRANTEE_ALL_USER,
                                                               permissions = [read, write]},
                                              #bucket_acl_info{user_id = ?GRANTEE_AUTHENTICATED_USER,
                                                               permissions = [full_control]}]
                                     }}),
            meck:new(leo_s3_auth, [no_link, non_strict]),
            meck:expect(leo_s3_auth, authenticate, 3, {ok, ["hoge"], undefined}),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC,Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST, ":8080/bucket?acl"]), [{"Date", Date}, {"Authorization","AWS auth:hoge"}]},
                                  [], [{full_result, false}]),
                ?assertEqual(200, SC),
                {_XmlDoc, Rest} = xmerl_scan:string(Body),
                ?assertEqual([], Rest)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                meck:unload(leo_s3_auth)
            end,
            ok
    end.
head_object_notfound_([_TermFun, Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node0, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect,
                          [leo_storage_handler_object, head, 2, {error, not_found}]),
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, head, 2, {error, not_found}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(head, {lists:append(["http://",
                                                       ?TARGET_HOST,
                                                       ":8080/a/b"]), [{"Date", Date}]},
                                  [], [{full_result, false}]),
                ?assertEqual(404, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

head_object_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,    [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, head, 2, {error, foobar}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(head, {lists:append(["http://",
                                                       ?TARGET_HOST,
                                                       ":8080/a/b"]), [{"Date", Date}]},
                                  [], [{full_result, false}]),
                ?assertEqual(500, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

head_object_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, head, 2,
                           {ok, #?METADATA{
                                    key =  <<"a/b/c/d.png">>,
                                    addr_id    = 0,
                                    ksize      = 4,
                                    dsize      = 16384,
                                    meta       = <<>>,
                                    msize      = 0,
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 1,
                                    clock      = 63511805822,
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   }
                           }]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {{_, SC, _}, Headers, _Body}} =
                    httpc:request(head, {lists:append(["http://",
                                                       ?TARGET_HOST,
                                                       ":8080/a/b/c/d.png"]), [{"Date", Date}, {"connection", "close"}]}, [], []),
                %% https://github.com/leo-project/leofs/issues/489#issuecomment-265389401
                %% exists only content-length header
                ?assertEqual({"content-length", "16384"}, lists:keyfind("content-length", 1, Headers)),
                ?assertEqual(false, lists:keyfind("transfer-encoding", 1, Headers)),
                ?assertEqual(200, SC)
                %% ?assertEqual("16384", proplists:get_value("content-length", Headers))
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

get_object_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 3, {error, foobar}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]), [{"Date", Date}]}, [], [{full_result, false}]),

                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_InternalError,
                                     ?XML_ERROR_MSG_InternalError,
                                     "a/b.png", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(500, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

get_object_invalid_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 3, {error, foobar}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(head, {lists:append(["http://",
                                                       ?TARGET_HOST,
                                                       ":8080/"]), [{"Date", Date}, {"Host", ""}]}, [{version, "HTTP/1.0"}], [{full_result, false}]),

                ?assertEqual(400, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

get_object_notfound_([_TermFun, Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node0, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect,
                          [leo_storage_handler_object, get, 3, {error, not_found}]),
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 3, {error, not_found}]),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b/c.png"]), [{"Date", Date}]}, [], [{full_result, false}]),

                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_NoSuchKey,
                                     ?XML_ERROR_MSG_NoSuchKey,
                                     "a/b/c.png", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(404, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

get_object_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 3,
                           {ok, #?METADATA{
                                    key =  <<"">>,
                                    addr_id    = 0,
                                    ksize      = 4,
                                    dsize      = 4,
                                    meta       = <<>>,
                                    msize      = 0,
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 1,
                                    clock      = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   },
                            <<"body">>}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {{_, SC, _}, Headers, Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]), [{"Date", Date}, {"connection", "close"}]}, [], []),
                ?assertEqual(200, SC),
                ?assertEqual("body", Body),
                ?assertEqual(undefined, proplists:get_value("X-From-Cache", Headers))
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

get_object_cmeta_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),

            CMetaBin = term_to_binary([{<<"x-amz-meta-test">>, <<"custom metadata">>}]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 3,
                           {ok, #?METADATA{
                                    key =  <<"">>,
                                    addr_id    = 0,
                                    ksize      = 4,
                                    dsize      = 4,
                                    meta       = CMetaBin,
                                    msize      = byte_size(CMetaBin),
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 1,
                                    clock      = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   },
                            <<"body">>}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {{_, SC, _}, Headers, Body}} =
                    httpc:request(get, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]), [{"Date", Date}, {"connection", "close"}]}, [], []),
                ?assertEqual(200, SC),
                ?assertEqual("body", Body),
                ?assertEqual(undefined, proplists:get_value("X-From-Cache", Headers)),
                ?assertEqual("custom metadata", proplists:get_value("x-amz-meta-test", Headers))
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

range_object_normal1_([_TermFun, _Node0, Node1]) ->
    range_object_base([_TermFun, _Node0, Node1], "bytes=1-2").
range_object_normal2_([_TermFun, _Node0, Node1]) ->
    range_object_base([_TermFun, _Node0, Node1], "bytes=1-").
range_object_normal3_([_TermFun, _Node0, Node1]) ->
    range_object_base([_TermFun, _Node0, Node1], "bytes=-1").

range_object_base([_TermFun, _Node0, Node1], RangeValue) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, head, 2,
                           {ok, #?METADATA{
                                    key =  <<"a/b.png">>,
                                    addr_id    = 0,
                                    ksize      = 4,
                                    dsize      = 16384,
                                    meta       = <<>>,
                                    msize      = 0,
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 1,
                                    clock      = 63505750315,
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   }
                           }]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, get, 5,
                           {ok, #?METADATA{
                                    key =  <<"">>,
                                    addr_id    = 0,
                                    ksize      = 2,
                                    dsize      = 2,
                                    meta       = <<>>,
                                    msize      = 0,
                                    csize      = 0,
                                    cnumber    = 0,
                                    cindex     = 0,
                                    offset     = 1,
                                    clock      = calendar:datetime_to_gregorian_seconds(erlang:universaltime()),
                                    timestamp  = 19740926,
                                    checksum   = 0,
                                    ring_hash  = 0,
                                    cluster_id = [],
                                    ver = 0,
                                    del = ?DEL_FALSE
                                   },
                            <<"od">>}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {{_, SC, _}, _Headers, Body}} =
                    httpc:request(get,
                                  {lists:append(["http://",
                                                 ?TARGET_HOST,
                                                 ":8080/a/b.png"]),
                                   [{"Date", Date},{"connection", "close"},{"range", RangeValue}]}, [], []),
                ?assertEqual(206, SC),
                ?assertEqual("od", Body)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

delete_object_notfound_([_TermFun, Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node0, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect,
                          [leo_storage_handler_object, delete, 2, {error, not_found}]),
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, delete, 2, {error, not_found}]),


            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(delete,
                                  {lists:append(["http://",
                                                 ?TARGET_HOST,
                                                 ":8080/a/b.png"]),
                                   [{"Date", Date}, {"Authorization","auth"}]}, [], [{full_result, false}]),
                ?assertEqual(204, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

delete_object_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, delete, 2, {error, foobar}]),


            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(delete, {lists:append(["http://",
                                                         ?TARGET_HOST,
                                                         ":8080/a/b.png"]),
                                           [{"Date", Date}, {"Authorization","auth"}]}, [], [{full_result, false}]),

                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_InternalError,
                                     ?XML_ERROR_MSG_InternalError,
                                     "a/b.png", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(500, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

delete_object_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, delete, 2, ok]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(delete, {lists:append(["http://",
                                                         ?TARGET_HOST,
                                                         ":8080/a/b.png"]),
                                           [{"Date", Date}, {"Authorization","auth"}]}, [], [{full_result, false}]),
                ?assertEqual(204, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

-undef(EUNIT_DEBUG_VAL_DEPTH).
-define(EUNIT_DEBUG_VAL_DEPTH, 1024).
put_object_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, put, 2, {error, foobar}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, Body}} =
                    httpc:request(put, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]),
                                        [{"Date", Date}, {"Authorization","auth"}], "image/png", "body"},
                                  [], [{full_result, false}]),
                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_InternalError,
                                     ?XML_ERROR_MSG_InternalError,
                                     "a/b.png", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(500, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

put_object_error_metadata_too_large_([_TermFun, _Node0, _Node1]) ->
    fun() ->
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                Dummy = lists:flatten(["test" || _ <- lists:seq(1, ?HTTP_METADATA_LIMIT * 2 div 4)]),
                %Dummy = crypto:rand_bytes(?HTTP_METADATA_LIMIT * 2),
                {ok, {SC, Body}} =
                    httpc:request(put, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]),
                                        [{"Date", Date}, {"Authorization","auth"}, {"x-amz-meta-test", Dummy}], "image/png", "body"},
                                  [], [{full_result, false}]),
                %% req id is empty for now
                Xml = io_lib:format(?XML_ERROR,
                                    [?XML_ERROR_CODE_MetadataTooLarge,
                                     ?XML_ERROR_MSG_MetadataTooLarge,
                                     "a/b.png", ""]),
                ?assertEqual(erlang:list_to_binary(Xml), erlang:list_to_binary(Body)),
                ?assertEqual(400, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            end,
            ok
    end.

put_object_normal1_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, put, 2, {ok, 1}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(put, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/a/b.png"]),
                                        [{"Date", Date}, {"Authorization","auth"}], "image/png", "body"},
                                  [], [{full_result, false}]),
                ?assertEqual(200, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

-define(CHUNKSIZE, 4096).
-define(AWSCHUNKEDSIZE, 65536).

gen_chunks(PrevSign, SignHead, SignKey, 0, Acc) ->
    Bin = <<>>,
    {Chunk, _Signature} = compute_chunk(PrevSign, SignHead, SignKey, Bin),
    <<Acc/binary, Chunk/binary>>;
gen_chunks(PrevSign, SignHead, SignKey, Remain, Acc) when Remain < ?CHUNKSIZE ->
    Bin = crypto:rand_bytes(Remain),
    {Chunk, Signature} = compute_chunk(PrevSign, SignHead, SignKey, Bin),
    gen_chunks(Signature, SignHead, SignKey, 0, <<Acc/binary, Chunk/binary>>);
gen_chunks(PrevSign, SignHead, SignKey, Remain, Acc) ->
    Bin = crypto:rand_bytes(?CHUNKSIZE),
    {Chunk, Signature} = compute_chunk(PrevSign, SignHead, SignKey, Bin),
    gen_chunks(Signature, SignHead, SignKey, Remain - ?CHUNKSIZE, <<Acc/binary, Chunk/binary>>).

compute_chunk(PrevSign, SignHead, SignKey, Bin) ->
    SizeHex = leo_hex:integer_to_hex(byte_size(Bin), 6),
    ChunkHashBin = leo_hex:binary_to_hexbin(crypto:hash(sha256, Bin)),
    BinToSign = <<"AWS4-HMAC-SHA256-PAYLOAD\n",
                  SignHead/binary,
                  PrevSign/binary,  "\n",
                  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855\n",
                  ChunkHashBin/binary>>,
    Signature = crypto:hmac(sha256, SignKey, BinToSign),
    Sign = leo_hex:binary_to_hexbin(Signature),
    SizeHexBin = list_to_binary(SizeHex),
    Chunk = <<SizeHexBin/binary, ";", "chunk-signature=", Sign/binary, "\r\n",
              Bin/binary, "\r\n">>,
    {Chunk, Sign}.

put_object_aws_chunked_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, put, 2, {ok, 1}]),
            meck:new(leo_s3_auth, [no_link, non_strict]),
            Signature = <<"642797dcfdf817ac23b553420f52c160847d3747b2e86e5ac9d07cc5e7f60f63">>,
            SignHead = <<"20150706T051217Z\n20150706/us-east-1/s3/aws4_request\n">>,
            SignKey = <<"2040321d898be34c82d1db9e132124f11eb18a3d21569d1ed58b460b88954ac7">>,
            meck:expect(leo_s3_auth, authenticate, 3, {ok, <<"05236">>, {Signature, SignHead, SignKey}}),
            meck:expect(leo_s3_bucket, get_acls, 1, not_found),
            Chunks = gen_chunks(Signature, SignHead, SignKey, ?AWSCHUNKEDSIZE, <<>>),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(put, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/testjv4/testFile.large.one"]),
                                        [{"Date", Date},
                                         {"authorization","AWS4-HMAC-SHA256 Credential=05236/20150706/us-east-1/s3/aws4_request, SignedHeaders=content-length, Signature=642797dcfdf817ac23b553420f52c160847d3747b2e86e5ac9d07cc5e7f60f63"},
                                         {"x-amz-content-sha256", "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"},
                                         {"x-amz-decoded-content-length", integer_to_list(?AWSCHUNKEDSIZE)}
                                        ], "image/png", Chunks},
                                  [], [{full_result, false}]),
                ?assertEqual(200, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
                ok = meck:unload(leo_s3_auth)
            end,
            ok
    end.

put_object_aws_chunked_error_([_TermFun, _Node0, Node1]) ->
    fun() ->
            ok = rpc:call(Node1, meck, new,
                          [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect,
                          [leo_storage_handler_object, put, 2, {ok, 1}]),
            meck:new(leo_s3_auth, [no_link, non_strict]),
            Signature = <<"642797dcfdf817ac23b553420f52c160847d3747b2e86e5ac9d07cc5e7f60f63">>,
            SignatureI = <<"incorrect">>,
            SignHead = <<"20150706T051217Z\n20150706/us-east-1/s3/aws4_request\n">>,
            SignKey = <<"2040321d898be34c82d1db9e132124f11eb18a3d21569d1ed58b460b88954ac7">>,

            meck:expect(leo_s3_auth, authenticate, 3, {ok, <<"05236">>, {Signature, SignHead, SignKey}}),
            meck:expect(leo_s3_bucket, get_latest_bucket, 1, not_found),

            Chunks = gen_chunks(SignatureI, SignHead, SignKey, ?AWSCHUNKEDSIZE, <<>>),
            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                {ok, {SC, _Body}} =
                    httpc:request(put, {lists:append(["http://",
                                                      ?TARGET_HOST,
                                                      ":8080/testjv4/testFile.large.one"]),
                                        [{"Date", Date},
                                         {"authorization","AWS4-HMAC-SHA256 Credential=05236/20150706/us-east-1/s3/aws4_request, SignedHeaders=content-length, Signature=642797dcfdf817ac23b553420f52c160847d3747b2e86e5ac9d07cc5e7f60f63"},
                                         {"x-amz-content-sha256", "STREAMING-AWS4-HMAC-SHA256-PAYLOAD"},
                                         {"x-amz-decoded-content-length", integer_to_list(?AWSCHUNKEDSIZE)}
                                        ], "image/png", Chunks},
                                  [], [{full_result, false}]),
                ?assertEqual(403, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object]),
                ok = meck:unload(leo_s3_auth)
            end,
            ok
    end.

%% proper_([_TermFun, _Node0, _Node1]) ->
%%     {timeout, 600, ?_assertEqual(true, leo_gateway_web_prop:test())}.

-endif.
