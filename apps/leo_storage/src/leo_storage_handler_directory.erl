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
%% @doc Handling a directory
%% @end
%%======================================================================
-module(leo_storage_handler_directory).

-include("leo_storage.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([find_by_parent_dir/4
        ]).

-define(DEF_MAX_KEYS, 1000).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% @doc Find by index from the backenddb.
%%
-spec(find_by_parent_dir(ParentDir, Delimiter, Marker, MaxKeys) ->
             {ok, list()} |
             {error, any()} when ParentDir::binary(),
                                 Delimiter::binary()|null,
                                 Marker::binary()|null,
                                 MaxKeys::integer()).
find_by_parent_dir(ParentDir, _Delimiter, Marker, MaxKeys) ->
    NewMaxKeys = case is_integer(MaxKeys) of
                     true  -> MaxKeys;
                     false -> ?DEF_MAX_KEYS
                 end,
    NewMarker  = case is_binary(Marker) of
                     true  -> Marker;
                     false -> <<>>
                 end,

    {ok, Members} = leo_redundant_manager_api:get_members(),
    Nodes = lists:foldl(fun(#member{node  = Node,
                                    state = ?STATE_RUNNING}, Acc) ->
                                [Node|Acc];
                           (_, Acc) ->
                                Acc
                        end, [], Members),

    {ResL, BadNodes} = rpc:multicall(Nodes, leo_storage_handler_object,
                                     prefix_search,
                                     [ParentDir, NewMarker, NewMaxKeys],
                                     ?DEF_REQ_TIMEOUT),
    Errors = [Cause || {error, Cause} <- ResL],
    HasBadNodes = (BadNodes /= []),
    HasErrors = (Errors /= []),

    case (HasErrors orelse HasBadNodes) of
        true ->
            ?warn("find_by_parent_dir/4",
                  [{errors, Errors}, {bad_nodes, BadNodes},
                   {cause, ?ERROR_COULD_NOT_GET_METADATAS}]),
            {error, unavailable};
        false ->
            case lists:foldl(
                   fun({ok, List}, Acc_1) ->
                           lists:foldl(
                             fun(#?METADATA{key = Key} = Metadata, Acc_2) ->
                                     case lists:keyfind(Key, 2, Acc_2) of
                                         false ->
                                             [Metadata|Acc_2];
                                         #?METADATA{clock = Clock}
                                           when Metadata#?METADATA.clock > Clock ->
                                             Acc_3 = lists:keydelete(Key, 2, Acc_2),
                                             [Metadata|Acc_3];
                                         _ ->
                                             Acc_2
                                     end
                             end, Acc_1, List);
                      (_, Acc_1) ->
                           Acc_1
                   end, [], ResL) of
                [] ->
                    {ok, []};
                List ->
                    {ok, lists:sublist(
                           ordsets:from_list(
                             lists:flatten(List)), NewMaxKeys)}
            end
    end.
