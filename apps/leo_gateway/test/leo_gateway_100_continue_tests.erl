%%====================================================================
%%
%% Leo Gateway - 100-Continue Tests
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
%% @doc Tests for HTTP 100-Continue (Expect header) handling
%%
%% This test module verifies that Cowboy correctly handles the
%% HTTP Expect: 100-continue header, which is used by clients like
%% boto3 (AWS SDK for Python) when uploading objects.
%%
%% See: issue-bad-request.md for the original issue description.
%% @end
%%====================================================================
-module(leo_gateway_100_continue_tests).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TARGET_HOST, "localhost").
-define(TARGET_PORT, 12346).      % Use different port from other tests
-define(TARGET_SSL_PORT, 12347).  % Use different SSL port from other tests

%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).

expect_100_continue_test_() ->
    {setup,
     fun setup/0,
     fun teardown/1,
     fun gen_tests/1}.

gen_tests(Arg) ->
    lists:map(fun(Test) -> Test(Arg) end,
              [fun put_object_without_expect_/1,
               fun put_object_with_expect_100_continue_/1,
               fun put_object_with_expect_100_continue_large_body_/1,
               fun put_object_with_expect_raw_socket_/1
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

setup() ->
    %% Logger setup
    logger:set_primary_config(level, info),

    io:format(user, "Setting up 100-continue tests...~n", []),
    [] = os:cmd("epmd -daemon"),
    {ok, Hostname} = inet:gethostname(),
    NetKernelNode = list_to_atom("netkernel_100c@" ++ Hostname),
    net_kernel:start([NetKernelNode, shortnames]),
    inets:start(),

    %% Start peer nodes
    Unique = erlang:unique_integer([positive]),
    Name0 = list_to_atom("storage_100c_" ++ integer_to_list(Unique) ++ "_0"),
    Name1 = list_to_atom("storage_100c_" ++ integer_to_list(Unique) ++ "_1"),
    {ok, Peer0, Node0} = peer:start_link(#{name => Name0, connection => standard_io}),
    {ok, Peer1, Node1} = peer:start_link(#{name => Name1, connection => standard_io}),

    %% Add code paths to peer nodes
    CodePaths = code:get_path(),
    lists:foreach(fun(P) -> rpc:call(Node0, code, add_patha, [P]) end, CodePaths),
    lists:foreach(fun(P) -> rpc:call(Node1, code, add_patha, [P]) end, CodePaths),

    %% Start meck on peer nodes
    {ok, _} = rpc:call(Node0, application, ensure_all_started, [meck]),
    {ok, _} = rpc:call(Node1, application, ensure_all_started, [meck]),
    put(peer_pids, [Peer0, Peer1]),

    ok = leo_misc:init_env(),

    %% Setup mocks
    meck:new(leo_redundant_manager_api, [non_strict]),
    meck:expect(leo_redundant_manager_api, get_redundancies_by_key,
                fun(_Method, _Key) ->
                        {ok, #redundancies{id = 0,
                                           nodes = [#redundant_node{node = Node0, available = true},
                                                    #redundant_node{node = Node1, available = true}],
                                           n = 2, r = 1, w = 1, d = 1}}
                end),

    meck:new(leo_s3_endpoint, [non_strict]),
    meck:expect(leo_s3_endpoint, get_endpoints, 0, {ok, [{endpoint, <<"localhost">>, 0}]}),

    meck:new(leo_s3_bucket, [non_strict]),
    meck:expect(leo_s3_bucket, get_latest_bucket,
                fun(_BucketName) ->
                        {ok, #?BUCKET{name = _BucketName,
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
    ok = rpc:call(Node0, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node0, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),
    ok = rpc:call(Node1, meck, new,    [leo_metrics_req, [no_link, non_strict]]),
    ok = rpc:call(Node1, meck, expect, [leo_metrics_req, notify, fun(_) -> ok end]),

    ok = file:write_file("./server_cert.pem", ?SSL_CERT_DATA),
    ok = file:write_file("./server_key.pem",  ?SSL_KEY_DATA),

    application:start(leo_cache),
    leo_cache_api:start(),

    leo_pod:start_link(?POD_LOH_WORKER,
                       ?env_loh_put_worker_pool_size(),
                       ?env_loh_put_worker_buffer_size(),
                       leo_large_object_worker, [],
                       fun(_) -> void end),

    %% Start Cowboy
    application:start(crypto),
    application:start(ranch),
    application:start(cowboy),

    {ok, Options} = leo_gateway_app:get_options(),
    leo_gateway_http_commons:start(Options#http_options{port = ?TARGET_PORT,
                                                        ssl_port = ?TARGET_SSL_PORT,
                                                        is_compatible_with_s3_content_type = true}),

    io:format(user, "100-continue test server started on port ~p~n", [?TARGET_PORT]),
    [Node0, Node1].

teardown([_Node0, _Node1]) ->
    inets:stop(),
    net_kernel:stop(),

    %% Stop peer nodes
    case get(peer_pids) of
        Pids when is_list(Pids) ->
            lists:foreach(fun(Pid) -> peer:stop(Pid) end, Pids);
        _ -> ok
    end,

    meck:unload(),

    cowboy:stop_listener(leo_gateway_s3_api),
    cowboy:stop_listener(leo_gateway_s3_api_ssl),

    application:stop(crypto),
    application:stop(ranch),
    application:stop(cowboy),
    leo_cache_api:stop(),
    timer:sleep(250),
    ok.

%%--------------------------------------------------------------------
%% Test: PUT without Expect header (baseline test)
%%--------------------------------------------------------------------
put_object_without_expect_([Node0, Node1]) ->
    fun() ->
            io:format(user, "~n=== Test: PUT without Expect header ===~n", []),

            ok = rpc:call(Node0, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),
            ok = rpc:call(Node1, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),

            try
                Date = leo_http:rfc1123_date(leo_date:now()),
                Url = lists:append(["http://", ?TARGET_HOST, ":", integer_to_list(?TARGET_PORT), "/bucket/test-no-expect.txt"]),
                Body = <<"Hello, World! (no expect)">>,

                {ok, {SC, _ResponseBody}} =
                    httpc:request(put, {Url, [{"Date", Date}, {"Authorization", "auth"}],
                                       "text/plain", Body},
                                  [], [{full_result, false}]),

                io:format(user, "Response status: ~p~n", [SC]),
                ?assertEqual(200, SC)
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

%%--------------------------------------------------------------------
%% Test: PUT with Expect: 100-continue header (small body)
%%--------------------------------------------------------------------
put_object_with_expect_100_continue_([Node0, Node1]) ->
    fun() ->
            io:format(user, "~n=== Test: PUT with Expect: 100-continue (small body) ===~n", []),

            ok = rpc:call(Node0, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),
            ok = rpc:call(Node1, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),

            try
                %% Use raw socket to properly test 100-continue
                Body = <<"Hello, World! (with 100-continue)">>,
                BodyLen = byte_size(Body),

                {ok, Socket} = gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT,
                                               [binary, {active, false}, {packet, raw}]),

                %% Send request headers with Expect: 100-continue
                Date = leo_http:rfc1123_date(leo_date:now()),
                Request = io_lib:format(
                    "PUT /bucket/test-100-continue.txt HTTP/1.1\r\n"
                    "Host: ~s:~p\r\n"
                    "Date: ~s\r\n"
                    "Authorization: auth\r\n"
                    "Content-Type: text/plain\r\n"
                    "Content-Length: ~p\r\n"
                    "Expect: 100-continue\r\n"
                    "\r\n",
                    [?TARGET_HOST, ?TARGET_PORT, Date, BodyLen]),

                ok = gen_tcp:send(Socket, Request),
                io:format(user, "Sent request headers, waiting for 100 Continue...~n", []),

                %% Wait for 100 Continue response
                {ok, Response1} = gen_tcp:recv(Socket, 0, 5000),
                io:format(user, "Received response: ~p~n", [Response1]),

                %% Check for 100 Continue
                ?assertMatch({match, _}, re:run(Response1, "100 Continue", [caseless])),

                %% Send body after receiving 100 Continue
                io:format(user, "Sending body after 100 Continue...~n", []),
                ok = gen_tcp:send(Socket, Body),

                %% Wait for final response
                {ok, Response2} = gen_tcp:recv(Socket, 0, 5000),
                io:format(user, "Final response: ~p~n", [Response2]),

                %% Check for 200 OK
                ?assertMatch({match, _}, re:run(Response2, "200 OK|HTTP/1.1 200", [])),

                gen_tcp:close(Socket),
                io:format(user, "Test passed: 100-continue with small body works!~n", [])
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

%%--------------------------------------------------------------------
%% Test: PUT with Expect: 100-continue header (large body - 1MB)
%%--------------------------------------------------------------------
put_object_with_expect_100_continue_large_body_([Node0, Node1]) ->
    fun() ->
            io:format(user, "~n=== Test: PUT with Expect: 100-continue (1MB body) ===~n", []),

            ok = rpc:call(Node0, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),
            ok = rpc:call(Node1, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),

            try
                %% Generate 1MB body
                BodySize = 1024 * 1024,  % 1MB
                Body = crypto:strong_rand_bytes(BodySize),

                {ok, Socket} = gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT,
                                               [binary, {active, false}, {packet, raw},
                                                {send_timeout, 30000}]),

                %% Send request headers with Expect: 100-continue
                Date = leo_http:rfc1123_date(leo_date:now()),
                Request = io_lib:format(
                    "PUT /bucket/test-100-continue-large.bin HTTP/1.1\r\n"
                    "Host: ~s:~p\r\n"
                    "Date: ~s\r\n"
                    "Authorization: auth\r\n"
                    "Content-Type: application/octet-stream\r\n"
                    "Content-Length: ~p\r\n"
                    "Expect: 100-continue\r\n"
                    "\r\n",
                    [?TARGET_HOST, ?TARGET_PORT, Date, BodySize]),

                ok = gen_tcp:send(Socket, Request),
                io:format(user, "Sent request headers for 1MB upload, waiting for 100 Continue...~n", []),

                %% Wait for 100 Continue response
                {ok, Response1} = gen_tcp:recv(Socket, 0, 10000),
                io:format(user, "Received response: ~p~n", [Response1]),

                %% Check for 100 Continue
                ?assertMatch({match, _}, re:run(Response1, "100 Continue", [caseless])),

                %% Send body in chunks after receiving 100 Continue
                io:format(user, "Sending 1MB body after 100 Continue...~n", []),
                ok = gen_tcp:send(Socket, Body),

                %% Wait for final response (with longer timeout for large body)
                {ok, Response2} = gen_tcp:recv(Socket, 0, 30000),
                io:format(user, "Final response: ~p~n", [Response2]),

                %% Check for 200 OK
                ?assertMatch({match, _}, re:run(Response2, "200 OK|HTTP/1.1 200", [])),

                gen_tcp:close(Socket),
                io:format(user, "Test passed: 100-continue with 1MB body works!~n", [])
            catch
                throw:Reason ->
                    throw(Reason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

%%--------------------------------------------------------------------
%% Test: PUT with Expect: 100-continue using raw socket (boto3-like behavior)
%% This test simulates boto3's behavior where it waits for 100 Continue
%% before sending the body.
%%--------------------------------------------------------------------
put_object_with_expect_raw_socket_([Node0, Node1]) ->
    fun() ->
            io:format(user, "~n=== Test: PUT with Expect: 100-continue (boto3-like, raw socket) ===~n", []),

            ok = rpc:call(Node0, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node0, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),
            ok = rpc:call(Node1, meck, new, [leo_storage_handler_object, [no_link, non_strict]]),
            ok = rpc:call(Node1, meck, expect, [leo_storage_handler_object, put, 2, {ok, 1}]),

            try
                Body = <<"This is a test body for boto3-like 100-continue test">>,
                BodyLen = byte_size(Body),

                {ok, Socket} = gen_tcp:connect(?TARGET_HOST, ?TARGET_PORT,
                                               [binary, {active, false}, {packet, raw}]),

                %% Send request headers only (like boto3 does)
                Date = leo_http:rfc1123_date(leo_date:now()),
                Request = io_lib:format(
                    "PUT /bucket/boto3-test.txt HTTP/1.1\r\n"
                    "Host: ~s:~p\r\n"
                    "Date: ~s\r\n"
                    "Authorization: auth\r\n"
                    "Content-Type: text/plain\r\n"
                    "Content-Length: ~p\r\n"
                    "Expect: 100-continue\r\n"
                    "\r\n",
                    [?TARGET_HOST, ?TARGET_PORT, Date, BodyLen]),

                ok = gen_tcp:send(Socket, Request),
                io:format(user, "Sent headers only, simulating boto3 waiting for 100 Continue...~n", []),

                %% Wait for 100 Continue (with timeout like boto3 would)
                case gen_tcp:recv(Socket, 0, 5000) of
                    {ok, Response1} ->
                        io:format(user, "Received: ~s~n", [Response1]),

                        case re:run(Response1, "100 Continue", [caseless]) of
                            {match, _} ->
                                io:format(user, "Got 100 Continue, now sending body...~n", []),

                                %% Small delay to simulate network latency (boto3-like)
                                timer:sleep(10),

                                %% Send body
                                ok = gen_tcp:send(Socket, Body),

                                %% Wait for final response
                                {ok, Response2} = gen_tcp:recv(Socket, 0, 5000),
                                io:format(user, "Final response: ~s~n", [Response2]),

                                %% Verify success
                                ?assertMatch({match, _}, re:run(Response2, "200 OK|HTTP/1.1 200", [])),
                                io:format(user, "Test passed: boto3-like 100-continue works!~n", []);

                            nomatch ->
                                %% Maybe we got the full response directly
                                case re:run(Response1, "HTTP/1.1 [45]", []) of
                                    {match, _} ->
                                        io:format(user, "ERROR: Got error response instead of 100 Continue: ~s~n", [Response1]),
                                        ?assert(false);
                                    nomatch ->
                                        io:format(user, "Unexpected response: ~s~n", [Response1]),
                                        ?assert(false)
                                end
                        end;

                    {error, timeout} ->
                        io:format(user, "ERROR: Timeout waiting for 100 Continue~n", []),
                        ?assert(false);

                    {error, SocketError} ->
                        io:format(user, "ERROR: Socket error: ~p~n", [SocketError]),
                        ?assert(false)
                end,

                gen_tcp:close(Socket)
            catch
                throw:ThrowReason ->
                    throw(ThrowReason)
            after
                ok = rpc:call(Node0, meck, unload, [leo_storage_handler_object]),
                ok = rpc:call(Node1, meck, unload, [leo_storage_handler_object])
            end,
            ok
    end.

-endif.
