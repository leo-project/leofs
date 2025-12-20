%%======================================================================
%%
%% Leo Gateway
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
%% ---------------------------------------------------------------------
%% Leo Gateway - CIDR Utility Module
%% @doc CIDR parsing and IP address matching utilities for internal
%%      network authentication bypass.
%% @end
%%======================================================================
-module(leo_gateway_cidr).

-export([is_in_cidrs/2,
         is_in_cidr/2]).

-type cidr() :: {inet:ip4_address(), non_neg_integer()}.

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------

%% @doc Check if IP address is in any of the CIDR ranges
-spec is_in_cidrs(inet:ip4_address(), [cidr()]) -> boolean().
is_in_cidrs(_IP, []) ->
    false;
is_in_cidrs(IP, CIDRs) ->
    lists:any(fun(CIDR) -> is_in_cidr(IP, CIDR) end, CIDRs).


%% @doc Check if IP address is in CIDR range
%% Example: is_in_cidr({10,0,1,100}, {{10,0,0,0}, 8}) -> true
-spec is_in_cidr(inet:ip4_address(), cidr()) -> boolean().
is_in_cidr({A, B, C, D}, {{NA, NB, NC, ND}, Mask})
  when is_integer(A), is_integer(B), is_integer(C), is_integer(D),
       is_integer(NA), is_integer(NB), is_integer(NC), is_integer(ND),
       is_integer(Mask), Mask >= 0, Mask =< 32 ->
    IPInt = (A bsl 24) bor (B bsl 16) bor (C bsl 8) bor D,
    NetInt = (NA bsl 24) bor (NB bsl 16) bor (NC bsl 8) bor ND,
    ShiftBits = 32 - Mask,
    (IPInt bsr ShiftBits) =:= (NetInt bsr ShiftBits);
is_in_cidr(_, _) ->
    %% IPv6 or invalid format not supported
    false.


%%--------------------------------------------------------------------
%% EUnit Tests
%%--------------------------------------------------------------------
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

is_in_cidr_test_() ->
    [
     %% Basic tests
     ?_assertEqual(true, is_in_cidr({10, 0, 0, 1}, {{10, 0, 0, 0}, 8})),
     ?_assertEqual(true, is_in_cidr({10, 255, 255, 255}, {{10, 0, 0, 0}, 8})),
     ?_assertEqual(false, is_in_cidr({11, 0, 0, 1}, {{10, 0, 0, 0}, 8})),

     %% /16 network
     ?_assertEqual(true, is_in_cidr({172, 16, 0, 1}, {{172, 16, 0, 0}, 12})),
     ?_assertEqual(true, is_in_cidr({172, 31, 255, 255}, {{172, 16, 0, 0}, 12})),
     ?_assertEqual(false, is_in_cidr({172, 32, 0, 1}, {{172, 16, 0, 0}, 12})),

     %% /24 network
     ?_assertEqual(true, is_in_cidr({192, 168, 1, 1}, {{192, 168, 1, 0}, 24})),
     ?_assertEqual(true, is_in_cidr({192, 168, 1, 255}, {{192, 168, 1, 0}, 24})),
     ?_assertEqual(false, is_in_cidr({192, 168, 2, 1}, {{192, 168, 1, 0}, 24})),

     %% /32 (single host)
     ?_assertEqual(true, is_in_cidr({192, 168, 1, 100}, {{192, 168, 1, 100}, 32})),
     ?_assertEqual(false, is_in_cidr({192, 168, 1, 101}, {{192, 168, 1, 100}, 32})),

     %% /0 (all IPs)
     ?_assertEqual(true, is_in_cidr({1, 2, 3, 4}, {{0, 0, 0, 0}, 0})),
     ?_assertEqual(true, is_in_cidr({255, 255, 255, 255}, {{0, 0, 0, 0}, 0})),

     %% Loopback
     ?_assertEqual(true, is_in_cidr({127, 0, 0, 1}, {{127, 0, 0, 0}, 8})),
     ?_assertEqual(true, is_in_cidr({127, 255, 255, 255}, {{127, 0, 0, 0}, 8}))
    ].

is_in_cidrs_test_() ->
    CIDRs = [{{10, 0, 0, 0}, 8},
             {{172, 16, 0, 0}, 12},
             {{192, 168, 0, 0}, 16}],
    [
     ?_assertEqual(true, is_in_cidrs({10, 0, 0, 1}, CIDRs)),
     ?_assertEqual(true, is_in_cidrs({172, 16, 0, 1}, CIDRs)),
     ?_assertEqual(true, is_in_cidrs({192, 168, 1, 1}, CIDRs)),
     ?_assertEqual(false, is_in_cidrs({8, 8, 8, 8}, CIDRs)),
     ?_assertEqual(false, is_in_cidrs({1, 2, 3, 4}, []))
    ].

-endif.
