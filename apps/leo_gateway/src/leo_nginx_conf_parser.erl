%%======================================================================
%%
%% Leo Gateway
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
%% ---------------------------------------------------------------------
%% Leo Gateway Parser for Nginx Configuration File
%%
%% @doc Parser of Nginx configuration file
%% @end
%%======================================================================
-module(leo_nginx_conf_parser).

%% External exports
-export([parse/1, get_custom_headers/2]).

-include_lib("eunit/include/eunit.hrl").

%% regular expressions to retrieve custom header information
-define(REGEX_LOCATION_BLOCK, "location\s+([-~%\._/0-9a-zA-Z]+)\s+{([^}]+)}").
-define(REGEX_KEY_VALUE_PAIR, "\s+([-~%\._/0-9a-zA-Z]+)\s+([^;]+);").
-define(REGEX_EXPIRE_VALUE, "(?:([0-9]+)d)?(?:([0-9]+)h)?(?:([0-9]+)m)?(?:([0-9]+)s)?|(-?[0-9]+)").

%% @doc Parse a nginx configuration file to get custom header settings.
-spec(parse(FileName) ->
             {ok, list()} |
             not_found |
             {error, any()} when FileName::file:name_all()).
parse(FileName) ->
    parse_1(file:read_file(FileName)).

%% @doc Get custom HTTP headers based on the return value of parse function
-spec(get_custom_headers(Path, ParseResult) ->
             {ok, list()} when Path::binary(), ParseResult::list()).
get_custom_headers(_Path, undefined) ->
    {ok, []};
get_custom_headers(Path, ParseResult) ->
    Ret = match_path_pattern(Path, ParseResult),
    case Ret of
        undefined ->
            {ok, []};
        Value ->
            make_result_headers(Value, [])
    end.

%%--------------------------------------------------------------------
%% Internal Functions.
%%--------------------------------------------------------------------
%% @private
match_path_pattern(_Path, []) ->
    undefined;
match_path_pattern(Path, [{Prefix, Value}|T]) ->
    case binary:match(Path, Prefix) of
        nomatch ->
            match_path_pattern(Path, T);
        {0, _} ->
            Value;
        {_, _} ->
            match_path_pattern(Path, T)
    end.

%% @private
make_result_headers([], Acc) ->
    {ok, Acc};
make_result_headers([{<<"add_header">>, KeyValuePair}|T], Acc) ->
    [Key, Val|_] = binary:split(KeyValuePair, <<" ">>),
    NewAcc = case Key of
                 <<"Cache-Control">> ->
                     merge_cache_control_values(Val, Acc);
                 _ ->
                     [{Key, Val}|Acc]
             end,
    make_result_headers(T, NewAcc);
make_result_headers([{<<"expires">>, ExpireVal}|T], Acc) ->
    ExpireInSec = normalize_expires(ExpireVal),
    {_, CCVal}= make_cache_control_header(ExpireInSec),
    NewAcc = merge_cache_control_values(CCVal, Acc),
    make_result_headers(T, NewAcc);
make_result_headers([{_, _KeyValuePair}|T], Acc) ->
    make_result_headers(T, Acc).

%% @private
normalize_expires(ExpireVal) ->
    {ok, MP} = re:compile(?REGEX_EXPIRE_VALUE),
    case re:run(ExpireVal, MP, [notempty,{capture, all_but_first, binary}]) of
        {error, ErrType} ->
            {error, ErrType};
        nomatch ->
            not_found;
        {match, [_,_,_,_,Digit|_]} ->
            %% Consit of only digits
            erlang:binary_to_integer(Digit);
        {match, [Day, Hour, Minute, Second|_]} ->
            %% Follow the below format
            %% For example, `1d1h30m15s` stands for 1 day, 1 hour, 30 minutes, 15 secs
            %% so that in order to deal with that value at max-age field,
            %% that should be converted to `5415` in seconnds
            day2int_in_sec(Day) +
                hour2int_in_sec(Hour) +
                minute2int_in_sec(Minute) +
                sec2int_in_sec(Second);
        {match, [Day, Hour, Minute|_]} ->
            day2int_in_sec(Day) +
                hour2int_in_sec(Hour) +
                minute2int_in_sec(Minute) ;
        {match, [Day, Hour|_]} ->
            day2int_in_sec(Day) +
                hour2int_in_sec(Hour);
        {match, [Day|_]} ->
            day2int_in_sec(Day);
        {match, _} ->
            {error, invalid_format}
    end.

%% @private
day2int_in_sec(<<>>) ->
    0;
day2int_in_sec(BinDay) ->
    erlang:binary_to_integer(BinDay) * 86400.

hour2int_in_sec(<<>>) ->
    0;
hour2int_in_sec(BinHour) ->
    erlang:binary_to_integer(BinHour) * 3600.

minute2int_in_sec(<<>>) ->
    0;
minute2int_in_sec(BinMinute) ->
    erlang:binary_to_integer(BinMinute) * 60.

sec2int_in_sec(<<>>) ->
    0;
sec2int_in_sec(BinSec) ->
    erlang:binary_to_integer(BinSec).

%% @private
make_cache_control_header(ExpireVal) when is_integer(ExpireVal), ExpireVal >= 0 ->
    BinExpireVal = erlang:integer_to_binary(ExpireVal),
    {<<"Cache-Control">>, <<"max-age=", BinExpireVal/binary>>};
make_cache_control_header(ExpireVal) when is_integer(ExpireVal) ->
    {<<"Cache-Control">>, <<"no-cache">>}.

merge_cache_control_values(NewVal, List) ->
    case lists:keyfind(<<"Cache-Control">>, 1, List) of
        false ->
            [{<<"Cache-Control">>, NewVal}|List];
        {_, OldVal} ->
            List2 = lists:keydelete(<<"Cache-Control">>, 1, List),
            [{<<"Cache-Control">>, <<OldVal/binary, <<", ">>/binary, NewVal/binary>>}|List2]
    end.

%% @private
parse_1({error, Reason}) ->
    {error, Reason};
parse_1({ok, Binary}) ->
    {ok, MP1} = re:compile(?REGEX_LOCATION_BLOCK, [multiline]),
    {ok, MP2} = re:compile(?REGEX_KEY_VALUE_PAIR),
    case re:run(Binary, MP1, [global]) of
        {error, ErrType} ->
            {error, ErrType};
        nomatch ->
            not_found;
        {match, Captured} ->
            parse_2(Captured, Binary, MP2, [])
    end.

%% @private
parse_2([], _Bin, _MP2, Acc) ->
    {ok, Acc};
parse_2([H|T], Bin, MP2, Acc) ->
    case parse_3(H, Bin, MP2) of
        {error, Cause} ->
            {error, Cause};
        not_found ->
            parse_2(T, Bin, MP2, Acc);
        Ret ->
            parse_2(T, Bin, MP2, [Ret|Acc])
    end.

%% @private
parse_3([_, {KeyPos, KeyLen}, {LocPos, LocLen}|_T], Bin, MP2) ->
    Key = binary:part(Bin, KeyPos, KeyLen),
    Loc = binary:part(Bin, LocPos, LocLen),
    case parse_location_body(Loc, MP2) of
        {error, Cause} ->
            {error, Cause};
        not_found ->
            not_found;
        Ret ->
            {Key, Ret}
    end;
parse_3(_, _Bin, _MP2) ->
    {error, invalid_file_format}.

%% @private
parse_location_body(Binary, MP2) ->
    case re:run(Binary, MP2, [global]) of
        {error, ErrType} ->
            {error, ErrType};
        nomatch ->
            not_found;
        {match, Captured} ->
            parse_location_body_1(Captured, Binary, [])
    end.

%% @private
parse_location_body_1([], _Bin, Acc) ->
    Acc;
parse_location_body_1([H|T], Bin, Acc) ->
    case parse_location_body_2(H, Bin) of
        {error, Cause} ->
            {error, Cause};
        Ret ->
            parse_location_body_1(T, Bin, [Ret|Acc])
    end.

%% @private
parse_location_body_2([_, {KeyPos, KeyLen}, {ValPos, ValLen}|_T], Bin) ->
    Key = binary:part(Bin, KeyPos, KeyLen),
    Val = binary:part(Bin, ValPos, ValLen),
    {Key, Val};
parse_location_body_2(_, _Bin) ->
    {error, invalid_location_format}.

-ifdef(EUNIT).
normalize_expires_test() ->
    ?assertEqual(30, normalize_expires(<<"30">>)),
    ?assertEqual(-10, normalize_expires(<<"-10">>)),
    ?assertEqual(86400, normalize_expires(<<"1d">>)),
    ?assertEqual(7200, normalize_expires(<<"2h">>)),
    ?assertEqual(1800, normalize_expires(<<"30m">>)),
    ?assertEqual(40, normalize_expires(<<"40s">>)),
    ?assertEqual(90061, normalize_expires(<<"1d1h1m1s">>)),
    ?assertEqual(86460, normalize_expires(<<"1d1m">>)),
    ?assertEqual(3630, normalize_expires(<<"1h30s">>)),
    ?assertEqual(172845, normalize_expires(<<"2d45s">>)),
    ok.
-endif.
