%%======================================================================
%%
%% Leo Gateway Large Object Common Functions
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
%%======================================================================
-module(leo_large_object_commons).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([iterator_init/2, iterator_next/1, iterator_set_chunked/3]).
-export([delete_chunked_objects/1]).

-undef(DEF_SEPARATOR).
-define(DEF_SEPARATOR, <<"\n">>).

-record(iterator, {origin_key = <<>> :: binary(),
                   origin_total_len = 0 :: non_neg_integer(),
                   origin_cur_idx = 0 :: non_neg_integer(),
                   chunked_key = <<>> :: binary(),
                   chunked_total_len = 0 :: non_neg_integer(),
                   chunked_cur_idx   = 0 :: non_neg_integer()
                  }).

-opaque iterator() :: #iterator{}.
-export_type([iterator/0]).


%%====================================================================
%% API
%%====================================================================
%% @doc Remove chunked objects
-spec(delete_chunked_objects(Key) ->
             ok when Key:: binary()).
delete_chunked_objects(Key) ->
    case leo_gateway_rpc_handler:delete(
           << Key/binary, ?DEF_SEPARATOR/binary >>) of
        ok ->
            void;
        {error, Cause} ->
            ?error("delete_chunked_objects/1",
                   [{key, binary_to_list(Key)}, {cause, Cause}])
    end,
    ok.

%% @doc Initialize a iterator for keys of a large object
-spec(iterator_init(Key, Total) ->
             #iterator{} when Key  :: binary(),
                              Total:: non_neg_integer()).
iterator_init(Key, Total) ->
    Iterator = #iterator{origin_key = Key, origin_total_len = Total},
    Iterator.

%% @doc Retrieve a next key for the iterator
-spec(iterator_next(Iterator) ->
             {binary(), #iterator{}} when Iterator::#iterator{}).
iterator_next(#iterator{chunked_key = <<>>, origin_cur_idx = TotalLen,
                        origin_total_len = TotalLen} = Iterator) ->
    {<<>>, Iterator};

iterator_next(#iterator{chunked_key = <<>>, origin_key = Key,
                        origin_cur_idx = Index} = Iterator) ->
    IndexBin = list_to_binary(integer_to_list(Index + 1)),
    {<< Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
     Iterator#iterator{origin_cur_idx = Index + 1}};

iterator_next(#iterator{origin_cur_idx = Total,
                        origin_total_len = Total,
                        chunked_total_len = ChunkedTotal,
                        chunked_cur_idx = ChunkedTotal} = Iterator) ->
    {<<>>, Iterator};

iterator_next(#iterator{origin_key = Key,
                        origin_cur_idx = Index,
                        chunked_key = _Chunked_key,
                        chunked_total_len = ChunkedTotal,
                        chunked_cur_idx = ChunkedTotal} = Iterator) ->
    IndexBin = list_to_binary(integer_to_list(Index + 1)),
    {<< Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
     Iterator#iterator{origin_cur_idx = Index + 1,
                       chunked_key = <<>>,
                       chunked_total_len = 0,
                       chunked_cur_idx = 0}};

iterator_next(#iterator{chunked_key = Key,
                        chunked_cur_idx = Index} = Iterator) ->
    IndexBin = list_to_binary(integer_to_list(Index + 1)),
    {<< Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
     Iterator#iterator{chunked_cur_idx = Index + 1}}.


-spec(iterator_set_chunked(#iterator{}, binary(), non_neg_integer()) ->
             #iterator{}).
iterator_set_chunked(Iterator, ChunkedKey, ChunkedTotal) ->
    Iterator#iterator{chunked_key = ChunkedKey, chunked_total_len = ChunkedTotal}.


%%====================================================================
%% TEST
%%====================================================================
-ifdef(TEST).
iterator_test() ->
    I = iterator_init(<<"hoge">>, 5),
    {<<"hoge\n1">>, I2} = iterator_next(I),
    {<<"hoge\n2">>, I3} = iterator_next(I2),
    I4 = iterator_set_chunked(I3, <<"hoge\n2">>, 3),
    {<<"hoge\n2\n1">>, I5} = iterator_next(I4),
    {<<"hoge\n2\n2">>, I6} = iterator_next(I5),
    {<<"hoge\n2\n3">>, I7} = iterator_next(I6),
    {<<"hoge\n3">>, I8} = iterator_next(I7),
    {<<"hoge\n4">>, I9} = iterator_next(I8),
    {Key, I10} = iterator_next(I9),
    I11 = iterator_set_chunked(I10, Key, 2),
    {<<"hoge\n5\n1">>, I12} = iterator_next(I11),
    {<<"hoge\n5\n2">>, I13} = iterator_next(I12),
    {<<"">>, _} = iterator_next(I13),
    {<<"">>, _} = iterator_next(I10).
-endif.
