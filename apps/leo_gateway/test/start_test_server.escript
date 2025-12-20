#!/usr/bin/env escript
%%! -pa _build/test/lib/*/ebin -pa _build/default/lib/*/ebin

main([]) ->
    main(["8080"]);
main([PortStr]) ->
    Port = list_to_integer(PortStr),
    io:format("Starting 100-continue test server on port ~p...~n", [Port]),

    %% Ensure applications are started
    {ok, _} = application:ensure_all_started(ranch),
    {ok, _} = application:ensure_all_started(cowboy),

    %% Setup routes
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/[...]", test_handler, []}
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
    io:format("Press Ctrl+C to stop~n", []),
    io:format("========================================~n~n", []),

    %% Keep running
    receive
        stop -> ok
    end.
