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
%% Parser of Nginx configuration file
%% @doc
%% @end
%%====================================================================
-module(leo_nginx_conf_parser_tests).

-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TEST_CONF_DIR, "../priv/test/").
%%--------------------------------------------------------------------
%% TEST
%%--------------------------------------------------------------------
-ifdef(EUNIT).
parse_test() ->
    TestCases = [
            {"nginx01.conf", 
                [{<<"/bucket_a/static_private">>, 
                  [{<<"expires">>, <<"24h">>},
                   {<<"add_header">>, <<"Cache-Control private">>}]
                 },
                 {<<"/bucket_b/static_public_frequently_changed">>, 
                  [{<<"expires">>, <<"5m">>},
                   {<<"add_header">>, <<"Cache-Control public">>}]
                 }
                ]
            },
            {"nginx02.conf", 
                [{<<"/bucket_a/test.png">>, 
                  [{<<"expires">>, <<"@24h">>},
                   {<<"add_header">>, <<"X-Original-Header OriginalValue">>}
                  ]
                 },
                 {<<"/bucket_b/urlencoded/%20%30%31%32">>, 
                  [{<<"expires">>, <<"-1">>},
                   {<<"add_header">>, <<"Cache-Control no-cache">>}]
                 }
                ]
            },
            {"nginx03.conf", 
                [{<<"/bucket_a/test.png">>, 
                  [{<<"expires">>, <<"0">>},
                   {<<"add_header">>, <<"X-Original-Header1 OriginalValue1">>},
                   {<<"add_header">>, <<"X-Original-Header2 OriginalValue2">>},
                   {<<"add_header">>, <<"X-Original-Header3 OriginalValue3">>}
                  ]
                 },
                 {<<"/bucket_b/~reserved/">>, 
                  [{<<"expires">>, <<"epoch">>},
                   {<<"add_header">>, <<"Cache-Control no-cache">>}]
                 }
                ]
            }
    ],
    validate(TestCases).

validate([]) ->
    ok;
validate([{FileName, Expected}|T]) ->
    FilePath = filename:join(?TEST_CONF_DIR, FileName),
    {ok, Result} = leo_nginx_conf_parser:parse(FilePath),
    compare(Expected, Result),
    validate(T).

compare([], _) ->
    ok;
compare([{Location, ExpectedPairList}|T], Result) ->
    ResultPairList = proplists:get_value(Location, Result),
    compare_pairlist(ExpectedPairList, ResultPairList),
    compare(T, Result).

compare_pairlist([], _) ->
    ok;
compare_pairlist([{ExpKey, ExpVal}|ET], ResultPairList) ->
    RetList = proplists:lookup_all(ExpKey, ResultPairList),
    ?assertEqual(true, is_exist(ExpVal, RetList)),
    compare_pairlist(ET, ResultPairList).

is_exist(_Val, []) ->
    false;
is_exist(Val, [{_, Val}|_T]) ->
    true;
is_exist(Val, [{_, _Val}|T]) ->
    is_exist(Val, T).

-endif.
