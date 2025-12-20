%%====================================================================
%% Simple test server for 100-Continue testing
%%
%% This module provides a standalone Cowboy server for testing
%% the Expect: 100-continue header handling without requiring
%% the full LeoFS stack.
%%
%% Usage:
%%   cd apps/leo_gateway
%%   rebar3 shell
%%   > test_100_continue_server:start().
%%   > test_100_continue_server:stop().
%%====================================================================
-module(test_100_continue_server).

-export([start/0, start/1, stop/0]).
-export([init/2]).

-define(DEFAULT_PORT, 8080).

%% @doc Start the test server on default port 8080
start() ->
    start(?DEFAULT_PORT).

%% @doc Start the test server on specified port
start(Port) ->
    io:format("Starting 100-continue test server on port ~p...~n", [Port]),

    %% Ensure applications are started
    application:ensure_all_started(cowboy),

    %% Setup routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", ?MODULE, []}
        ]}
    ]),

    %% Start the listener
    {ok, _} = cowboy:start_clear(
        test_100_continue_listener,
        #{
            socket_opts => [{port, Port}],
            num_acceptors => 10
        },
        #{
            env => #{dispatch => Dispatch},
            request_timeout => 60000,
            idle_timeout => 120000
        }
    ),

    io:format("~n========================================~n", []),
    io:format("Test server started on http://localhost:~p~n", [Port]),
    io:format("========================================~n~n", []),
    io:format("Test with:~n", []),
    io:format("  curl -X PUT http://localhost:~p/test-bucket/test.txt -d 'hello'~n", [Port]),
    io:format("  curl -X PUT -H 'Expect: 100-continue' http://localhost:~p/test-bucket/test.txt -d 'hello'~n", [Port]),
    io:format("~nOr run:~n", []),
    io:format("  python test/test_100_continue_boto3.py -e http://localhost:~p -b test -a test -s test~n", [Port]),
    io:format("~n", []),
    ok.

%% @doc Stop the test server
stop() ->
    io:format("Stopping test server...~n", []),
    cowboy:stop_listener(test_100_continue_listener),
    io:format("Test server stopped.~n", []),
    ok.

%% @doc Cowboy handler init callback
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    Headers = cowboy_req:headers(Req0),

    %% Log the request
    io:format("~n--- Request ---~n", []),
    io:format("Method: ~s~n", [Method]),
    io:format("Path:   ~s~n", [Path]),

    %% Check for Expect header
    ExpectHeader = maps:get(<<"expect">>, Headers, undefined),
    io:format("Expect: ~p~n", [ExpectHeader]),

    %% Handle the request
    {ok, Req} = handle_request(Method, Req0),
    {ok, Req, State}.

handle_request(<<"GET">>, Req0) ->
    %% Handle GET - return simple response
    Body = <<"Hello from test server">>,
    Req = cowboy_req:reply(200,
        #{<<"content-type">> => <<"text/plain">>},
        Body, Req0),
    {ok, Req};

handle_request(<<"PUT">>, Req0) ->
    %% Handle PUT - read body (this triggers 100-continue handling)
    io:format("Reading body...~n", []),

    case cowboy_req:read_body(Req0) of
        {ok, Body, Req1} ->
            io:format("Body received: ~p bytes~n", [byte_size(Body)]),

            %% Generate ETag
            Hash = crypto:hash(md5, Body),
            ETag = iolist_to_binary(["\"", binary_to_hex(Hash), "\""]),

            Req = cowboy_req:reply(200,
                #{
                    <<"content-type">> => <<"application/xml">>,
                    <<"etag">> => ETag
                },
                <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><PutObjectResult></PutObjectResult>">>,
                Req1),
            {ok, Req};

        {more, Partial, Req1} ->
            %% Large body - read in chunks
            io:format("Large body, reading chunks...~n", []),
            read_body_loop(Partial, Req1)
    end;

handle_request(<<"DELETE">>, Req0) ->
    %% Handle DELETE
    io:format("Delete request~n", []),
    Req = cowboy_req:reply(204, #{}, <<>>, Req0),
    {ok, Req};

handle_request(<<"HEAD">>, Req0) ->
    %% Handle HEAD
    Req = cowboy_req:reply(200, #{}, <<>>, Req0),
    {ok, Req};

handle_request(_, Req0) ->
    %% Unknown method
    Req = cowboy_req:reply(405, #{}, <<"Method Not Allowed">>, Req0),
    {ok, Req}.

read_body_loop(Acc, Req0) ->
    case cowboy_req:read_body(Req0) of
        {ok, Body, Req1} ->
            FullBody = <<Acc/binary, Body/binary>>,
            io:format("Body complete: ~p bytes~n", [byte_size(FullBody)]),

            Hash = crypto:hash(md5, FullBody),
            ETag = iolist_to_binary(["\"", binary_to_hex(Hash), "\""]),

            Req = cowboy_req:reply(200,
                #{
                    <<"content-type">> => <<"application/xml">>,
                    <<"etag">> => ETag
                },
                <<"<?xml version=\"1.0\" encoding=\"UTF-8\"?><PutObjectResult></PutObjectResult>">>,
                Req1),
            {ok, Req};

        {more, Partial, Req1} ->
            NewAcc = <<Acc/binary, Partial/binary>>,
            io:format("Body chunk: ~p bytes (total: ~p)~n",
                      [byte_size(Partial), byte_size(NewAcc)]),
            read_body_loop(NewAcc, Req1)
    end.

binary_to_hex(Bin) ->
    << <<(hex_digit(H)), (hex_digit(L))>> || <<H:4, L:4>> <= Bin >>.

hex_digit(N) when N < 10 -> $0 + N;
hex_digit(N) -> $a + N - 10.
