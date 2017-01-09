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
%%======================================================================
-module(leo_nfs_file_handler).

-include("leo_gateway.hrl").
-include("leo_http.hrl").
-include("leo_nfs_proto3.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_object_storage/include/leo_object_storage.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("kernel/include/file.hrl").

-export([is_file/1, is_dir/1, list_dir/1, list_dir/2, rename/2]).
-export([write/4, read/3, trim/2]).
-export([path_to_dir/1, path_trim_trailing_sep/1, path_relative_to_abs/1,
         binary_is_contained/2, get_disk_usage/0]).

-undef(DEF_SEPARATOR).
-define(DEF_SEPARATOR, <<"\n">>).
-define(LEOFS_NUM_OF_LIST_DIR, 1000).


%% ---------------------------------------------------------------------
%% API-1
%% ---------------------------------------------------------------------
%% @doc Returns true if Path refers to a file, and false otherwise.
%%
-spec(is_file(binary()) -> boolean()).
is_file(Path) ->
    case leo_gateway_rpc_handler:head(Path) of
        {ok, #?METADATA{del = 0}} ->
            true;
        {ok,_} ->
            %% deleted(del = 1)
            false;
        _ ->
            false
    end.


%% @doc
%% Returns true if Path refers to a directory, and false otherwise.
-spec(is_dir(binary()) -> boolean()).
is_dir(Path) ->
    %% === For 1.4
    %% {ok, #redundancies{nodes = Redundancies}} =
    %%     leo_redundant_manager_api:get_redundancies_by_key(get, Path),
    %% leo_gateway_rpc_handler:invoke(Redundancies,
    %%                                leo_storage_handler_directory,
    %%                                is_dir,
    %%                                [Path],
    %%                                []).

    case list_dir(Path, false) of
        {ok, Meta} when is_list(Meta) =:= true andalso length(Meta) > 0 ->
            true;
        _Error ->
            false
    end.

%% @doc
%%
-spec(list_dir(binary()) ->
             {ok, list(#?METADATA{})}|{error, any()}).
list_dir(Path) ->
    list_dir(Path, true).

-spec(list_dir(binary(), boolean()) ->
             {ok, list(#?METADATA{})}|{error, any()}).
list_dir(Path, IncludeHiddenFiles) ->
    {ok, #redundancies{nodes = Redundancies}} =
        leo_redundant_manager_api:get_redundancies_by_key(get, Path),
    Modifier = case IncludeHiddenFiles of
                   true ->
                       fun list_dir_append_hidden_files/2;
                   false ->
                       void
               end,
    list_dir(Redundancies, Path, <<>>, [], Modifier).

list_dir(Redundancies, Path, Marker, Acc, Modifier) when is_function(Modifier) ->
    case leo_gateway_rpc_handler:invoke(
           Redundancies,
           leo_storage_handler_directory,
           find_by_parent_dir,
           [Path, <<"/">>, Marker, ?LEOFS_NUM_OF_LIST_DIR], []) of
        {ok, []} ->
            {ok, Modifier(Path, Acc)};
        {ok, Metadata} when is_list(Metadata) ->
            Last = lists:last(Metadata),
            TrimedKey = path_trim_trailing_sep(Last#?METADATA.key),
            case Marker of
                TrimedKey ->
                    {ok, Modifier(Path, Acc)};
                _Other ->
                    list_dir(Redundancies, Path, TrimedKey, Metadata ++ Acc, Modifier)
            end;
        Error ->
            ?error("list_dir/5", [{cause, Error}]),
            Error
    end;
list_dir(Redundancies, Path, Marker,_Acc,_Modifier) ->
    leo_gateway_rpc_handler:invoke(
      Redundancies,
      leo_storage_handler_directory,
      find_by_parent_dir,
      [Path, <<"/">>, Marker, ?LEOFS_NUM_OF_LIST_DIR], []).

%% @private
list_dir_append_hidden_files(BasePath, List) ->
    %% add current(.) and parent(..) directories
    CuurentDirKey = << BasePath/binary, <<".">>/binary >>,
    CurrentDir = #?METADATA{key = CuurentDirKey, dsize = -1},
    ParentDirKey = << BasePath/binary, <<"..">>/binary >>,
    ParentDir = #?METADATA{key = ParentDirKey, dsize = -1},
    [CurrentDir, ParentDir|List].


%% @doc Rename the file SrcKey to DstKey
%%
-spec(rename(binary(), binary()) ->
             ok |
             {error, not_found} |
             {error, ?ERR_TYPE_INTERNAL_ERROR} |
             {error, timeout}).
rename(SrcKey, DstKey) ->
    case leo_gateway_rpc_handler:get(SrcKey) of
        {ok, #?METADATA{cnumber = 0} = Metadata, RespObject} ->
            rename_1(DstKey, Metadata, RespObject);
        {ok, #?METADATA{cnumber = _TotalChunkedObjs} = Metadata,_RespObject} ->
            rename_large_object_1(DstKey, Metadata);
        Error ->
            Error
    end.

%% @private
-spec(rename_1(Key, Metadata, Bin) ->
             ok |
             {error, ?ERR_TYPE_INTERNAL_ERROR} |
             {error, timeout} when Key::binary(),
                                   Metadata::#?METADATA{},
                                   Bin::binary()).
rename_1(Key, Metadata, Bin) ->
    CMeta = Metadata#?METADATA.meta,
    case leo_gateway_rpc_handler:put(
           #put_req_params{path = Key,
                           body = Bin,
                           meta = CMeta,
                           msize = byte_size(CMeta),
                           dsize = byte_size(Bin)}) of
        {ok,_ETag} ->
            rename_2(Metadata);
        Error ->
            %%
            Error
    end.

%% @private
-spec(rename_2(Metadata) ->
             ok |
             {error, ?ERR_TYPE_INTERNAL_ERROR} |
             {error, timeout} when Metadata::#?METADATA{}).
rename_2(Metadata) ->
    case leo_gateway_rpc_handler:delete(Metadata#?METADATA.key) of
        ok ->
            ok;
        {error, not_found} ->
            ok;
        Error ->
            Error
    end.

%% @private
rename_large_object_1(Key, Metadata) ->
    %% Params to be applied with configurations about large object
    LargeObjectProp = ?env_large_object_properties(),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',
                                       LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    Params = #req_params{chunked_obj_len = ChunkedObjLen},
    case leo_gateway_http_commons:move_large_object(Metadata, Key, Params) of
        ok ->
            rename_large_object_2(Metadata);
        Error ->
            Error %% {error, ?ERR_TYPE_INTERNAL_ERROR} | {error, timeout}
    end.

%% @private
rename_large_object_2(Metadata) ->
    leo_large_object_commons:delete_chunked_objects(Metadata#?METADATA.key),
    catch leo_gateway_rpc_handler:delete(Metadata#?METADATA.key),
    ok.


%% -------------------------------------------------------------------
%% WRITE operation
%% -------------------------------------------------------------------
%% @doc Update a part of the file which start position is Start
%%      and the end position is End
-spec(write(binary(), pos_integer(), pos_integer(), binary()) ->
             ok | {error, any()}).
write(Key, Start, End, Bin) ->
    LargeObjectProp = ?env_large_object_properties(),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',
                                       LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    IsLarge = (End + 1) > ChunkedObjLen,
    case leo_gateway_rpc_handler:get(Key) of
        {ok, #?METADATA{cnumber = 0} = SrcMetadata, SrcObj} when IsLarge =:= true ->
            write_small2large(Key, Start, End, Bin, SrcMetadata, SrcObj);
        {ok, #?METADATA{cnumber = 0} = SrcMetadata, SrcObj} when IsLarge =:= false ->
            write_small2small(Key, Start, End, Bin, SrcMetadata, SrcObj);
        {ok, #?METADATA{cnumber = _CNum} = SrcMetadata, _} ->
            catch write_large2any(Key, Start, End, Bin, SrcMetadata);
        {error, not_found} when IsLarge =:= true ->
            write_nothing2large(Key, Start, End, Bin);
        {error, not_found} when IsLarge =:= false ->
            write_nothing2small(Key, Start, End, Bin);
        {error, Cause} ->
            ?debug("write/4","Cause: ~w",[Cause]),
            {error, Cause}
    end.

%% @private
write_small2large(Key, Start,_End, Bin, SrcMetadata, SrcObj) ->
    %% result to be merged with existing data blocks
    Bin_1 = case Start > SrcMetadata#?METADATA.dsize of
                true ->
                    << SrcObj/binary, 0:(8*(Start - SrcMetadata#?METADATA.dsize)), Bin/binary >>;
                false ->
                    << Head:Start/binary,_/binary >> = SrcObj,
                    << Head/binary, Bin/binary >>
            end,
    case large_obj_update(Key, Bin_1) of
        ok ->
            ok;
        Error ->
            Error
    end.

%% @private
write_small2small(Key, Start, End, Bin, SrcMetadata, SrcObj) ->
    %% result to be merged with existing data blocks
    Bin_1 = case Start > SrcMetadata#?METADATA.dsize of
                true ->
                    << SrcObj/binary, 0:(8*(Start - SrcMetadata#?METADATA.dsize)), Bin/binary >>;
                false ->
                    case (End + 1) < SrcMetadata#?METADATA.dsize of
                        true ->
                            << Head:Start/binary,_/binary >> = SrcObj,
                            << _:End/binary,_:1/binary, Tail/binary >> = SrcObj,
                            << Head/binary, Bin/binary, Tail/binary >>;
                        false ->
                            << Head:Start/binary,_/binary >> = SrcObj,
                            << Head/binary, Bin/binary >>
                    end
            end,
    case leo_gateway_rpc_handler:put(
           #put_req_params{path = Key,
                           body = Bin_1,
                           dsize = byte_size(Bin_1)}) of
        {ok,_} ->
            ok;
        Error ->
            Error
    end.

%% @private
write_large2any(Key, Start, End, Bin, SrcMetadata) ->
    LargeObjectProp = ?env_large_object_properties(),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',
                                       LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    IndexStart = Start div ChunkedObjLen + 1,
    IndexEnd = End div ChunkedObjLen + 1,
    LastChunkSize = case IndexStart =:= IndexEnd of
        true ->
            Offset = Start rem ChunkedObjLen,
            Size = End - Start + 1,
            case large_obj_partial_update(Key, Bin, IndexStart, Offset, Size) of
                {error, Cause} ->
                    throw({error, Cause});
                _ ->
                    Offset + Size
            end;
        false ->
            %% head
            HeadOffset = Start rem ChunkedObjLen,
            HeadSize = ChunkedObjLen - HeadOffset,
            << HeadBin:HeadSize/binary, Rest/binary >> = Bin,
            case large_obj_partial_update(Key, HeadBin, IndexStart, HeadOffset, HeadSize) of
                {error, Cause} ->
                    throw({error, Cause});
                _ -> nop
            end,
            %% middle
            Rest3 = lists:foldl(
                      fun(Index, << MidBin:ChunkedObjLen/binary, Rest2/binary >>) ->
                              case large_obj_partial_update(Key, MidBin, Index) of
                                  {error, Cause2} ->
                                      throw({error, Cause2});
                                  _ -> nop
                              end,
                              Rest2
                      end,
                      Rest,
                      lists:seq(IndexStart + 1, IndexEnd - 1)),
            %% tail
            TailOffset = 0,
            TailSize = End rem ChunkedObjLen + 1,
            case large_obj_partial_update(Key, Rest3, IndexEnd, TailOffset, TailSize) of
                {error, Cause3} ->
                    throw({error, Cause3});
                _ ->
                    TailSize
            end
    end,
    NumChunks = erlang:max(IndexEnd, SrcMetadata#?METADATA.cnumber),
    %% https://github.com/leo-project/leofs/issues/537
    %% calc total size
    TotalSize = ChunkedObjLen * (NumChunks - 1) + LastChunkSize,
    large_obj_partial_commit(Key, NumChunks, ChunkedObjLen, TotalSize).

%% @private
write_nothing2large(Key, Start,_End, Bin) ->
    Bin_1 = << 0:(Start*8), Bin/binary >>,
    case large_obj_update(Key, Bin_1) of
        ok ->
            ok;
        Error ->
            Error
    end.

%% @private
write_nothing2small(Key, Start,_End, Bin) ->
    %% zero pdding to be added until the position reached Start
    Bin_1 = << 0:(Start*8), Bin/binary >>,

    case leo_gateway_rpc_handler:put(
           #put_req_params{path = Key,
                           body = Bin_1,
                           dsize = byte_size(Bin)}) of
        {ok,_} ->
            ok;
        Error ->
            Error
    end.

%% @doc Update the whole file which is handled as a large object in LeoFS
%% @private
-spec(large_obj_update(binary(), binary()) ->
             ok | {error, any()}).
large_obj_update(Key, Bin) ->
    LargeObjectProp = ?env_large_object_properties(),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',
                                       LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    {ok, Handler} = leo_large_object_put_handler:start_link(Key, ChunkedObjLen),
    case catch leo_large_object_put_handler:put(Handler, Bin) of
        ok ->
            large_obj_commit(
              Handler, Key, byte_size(Bin), ChunkedObjLen);
        {_, Cause} ->
            ok = leo_large_object_put_handler:rollback(Handler),
            {error, Cause}
    end.

%% @private
large_obj_commit(Handler, Key, Size, ChunkedObjLen) ->
    case catch leo_large_object_put_handler:result(Handler) of
        {ok, #large_obj_info{length = TotalSize,
                             num_of_chunks = TotalChunks,
                             md5_context = Digest}} when Size == TotalSize ->
            Digest_1 = leo_hex:raw_binary_to_integer(Digest),
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key,
                                   body = ?BIN_EMPTY,
                                   dsize = Size,
                                   total_chunks = TotalChunks,
                                   csize = ChunkedObjLen,
                                   digest = Digest_1}) of
                {ok,_ETag} ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        {ok,_} ->
            {error, ?ERROR_NOT_MATCH_LENGTH};
        {_, Cause} ->
            {error, Cause}
    end.


%% @doc Update the chunked file which is a part of a large object in LeoFS
%% @private
-spec(large_obj_partial_update(binary(), binary(), pos_integer()) ->
             ok | {error, any()}).
large_obj_partial_update(Key, Bin, Index) ->
    IndexBin = list_to_binary(integer_to_list(Index)),
    Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,

    case leo_gateway_rpc_handler:put(
           #put_req_params{path = Key_1,
                           body = Bin,
                           dsize = byte_size(Bin),
                           cindex = Index}) of
        {ok,_ETag} ->
            ok;
        {error, Cause} ->
            ?error("large_obj_partial_update/3", [{key, Key_1}, {cause, Cause}]),
            {error, Cause}
    end.
large_obj_partial_update(Key, Data, Index, Offset, Size) ->
    IndexBin = list_to_binary(integer_to_list(Index)),
    Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
    case leo_gateway_rpc_handler:get(Key_1) of
        {ok, Metadata, Bin} ->
            Bin_1 = case Offset > Metadata#?METADATA.dsize of
                        true ->
                            << Bin/binary, 0:(8*(Offset - Metadata#?METADATA.dsize)), Data/binary >>;
                        false ->
                            case (Offset + Size + 1) < Metadata#?METADATA.dsize of
                                true ->
                                    End = Offset + Size,
                                    << Head:Offset/binary,_/binary >> = Bin,
                                    << _:End/binary, Tail/binary >> = Bin,
                                    << Head/binary, Data/binary, Tail/binary >>;
                                false ->
                                    << Head:Offset/binary,_/binary >> = Bin,
                                    << Head/binary, Data/binary >>
                            end
                    end,
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key_1,
                                   body = Bin_1,
                                   dsize = byte_size(Bin_1),
                                   cindex = Index}) of
                {ok,_ETag} ->
                    ok;
                {error, Cause} ->
                    ?error("large_obj_partial_update/5", [{key, Key_1}, {cause, Cause}]),
                    {error, Cause}
            end;
        {error, not_found} ->
            Bin_1 = << 0:(Offset*8), Data/binary >>,
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key_1,
                                   body = Bin_1,
                                   dsize = byte_size(Bin_1),
                                   cindex = Index}) of
                {ok,_ETag} ->
                    ok;
                {error, Cause} ->
                    ?error("large_obj_partial_update/5", [{key, Key_1}, {cause, Cause}]),
                    {error, Cause}
            end;
        {error, Cause} ->
            ?error("large_obj_partial_update/5", [{key, Key_1}, {cause, Cause}]),
            {error, Cause}
    end.


%% @doc Update the metadata of a large file to reflect the total file size
%% @todo Chucksum(MD5) also have to be updated
%% @private
-spec(large_obj_partial_commit(binary(), pos_integer(), pos_integer(), pos_integer()) ->
             ok | {error, any()}).
large_obj_partial_commit(Key, TotalChunks, ChunkSize, TotalSize) ->
    case leo_gateway_rpc_handler:put(
           #put_req_params{path = Key,
                           body = ?BIN_EMPTY,
                           dsize = TotalSize,
                           total_chunks = TotalChunks,
                           csize = ChunkSize,
                           digest = 0}) of
        {ok,_} ->
            ok;
        {error, Cause} = Error ->
            ?error("large_obj_partial_commit/4", [{key, Key}, {cause, Cause}]),
            Error
    end.

%% -------------------------------------------------------------------
%% READ operation
%% -------------------------------------------------------------------
%% @doc Retrieve a part of the file which start position is Start
%%      and the end position is End
-spec(read(binary(), pos_integer(), pos_integer()) ->
             {ok, #?METADATA{}, binary()}| {error, any()}).
read(Key, Start, End) ->
    case leo_gateway_rpc_handler:head(Key) of
        {ok, #?METADATA{dsize = ObjectSize} = Metadata} ->
            End2 = if ObjectSize - 1 > End ->
                           End;
                      true ->
                           ObjectSize - 1
                   end,
            case Metadata of
                #?METADATA{del = 0, cnumber = 0} ->
                    read_small(Key, Start, End2);
                #?METADATA{del = 0, cnumber = N, csize = CS} = Metadata ->
                    {NewStartPos, NewEndPos} = calc_pos(Start, End2, ObjectSize),
                    {CurPos, Index} = move_curpos2head(NewStartPos, CS, 0, 0),
                    {ok,_Pos, Bin} = read_large(Key, NewStartPos, NewEndPos, N, Index, CurPos, <<>>),
                    {ok, Metadata, Bin};
                _ ->
                    {error, not_found}
            end;
        Error ->
            Error
    end.

%% @private
read_small(Key, Start, End) ->
    case leo_gateway_rpc_handler:get(Key, Start, End) of
        {ok, Metadata, Bin} ->
            {ok, Metadata, Bin};
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
move_curpos2head(Start, ChunkedSize, CurPos, Idx) when (CurPos + ChunkedSize - 1) < Start ->
    move_curpos2head(Start, ChunkedSize, CurPos + ChunkedSize, Idx + 1);
move_curpos2head(_Start,_ChunkedSize, CurPos, Idx) ->
    {CurPos, Idx}.

%% @private
calc_pos(_StartPos, EndPos, ObjectSize) when EndPos < 0 ->
    NewStartPos = ObjectSize + EndPos,
    NewEndPos = ObjectSize - 1,
    {NewStartPos, NewEndPos};
calc_pos(StartPos, 0, ObjectSize) ->
    {StartPos, ObjectSize - 1};
calc_pos(StartPos, EndPos,_ObjectSize) ->
    {StartPos, EndPos}.

%% @private
read_large(_Key,_Start,_End, Total, Total, CurPos, Acc) ->
    {ok, CurPos, Acc};
read_large(_Key,_Start, End,_Total,_Index, CurPos, Acc) when CurPos > End ->
    {ok, CurPos, Acc};
read_large(Key, Start, End, Total, Index, CurPos, Acc) ->
    IndexBin = list_to_binary(integer_to_list(Index + 1)),
    Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
    case leo_gateway_rpc_handler:head(Key_1) of
        {ok, #?METADATA{cnumber = 0, dsize = CS}} ->
            {NewPos, Bin} = get_chunk(Key_1, Start, End, CurPos, CS),
            read_large(Key, Start, End, Total, Index + 1, NewPos, << Acc/binary, Bin/binary >>);
        {ok, #?METADATA{cnumber = GrandChildNum}} ->
            case read_large(Key_1, Start, End, GrandChildNum, 0, CurPos, Acc) of
                {ok, NewPos, NewAcc} ->
                    read_large(Key, Start, End, Total, Index + 1, NewPos, NewAcc);
                {error, Cause} ->
                    {error, Cause}
            end;
        {error, Cause} ->
            {error, Cause}
    end.

%% @private
get_chunk(_Key, Start,_End, CurPos, ChunkSize) when (CurPos + ChunkSize - 1) < Start ->
    %% skip proc
    {CurPos + ChunkSize, <<>>};
get_chunk(Key, Start, End, CurPos, ChunkSize) when CurPos >= Start andalso
                                                   (CurPos + ChunkSize - 1) =< End ->
    %% whole get
    case leo_gateway_rpc_handler:get(Key) of
        {ok,_Metadata, Bin} ->
            {CurPos + ChunkSize, Bin};
        Error ->
            Error
    end;
get_chunk(Key, Start, End, CurPos, ChunkSize) ->
    %% partial get
    StartPos = case Start =< CurPos of
                   true -> 0;
                   false -> Start - CurPos
               end,
    EndPos = case (CurPos + ChunkSize - 1) =< End of
                 true -> ChunkSize - 1;
                 false -> End - CurPos
             end,
    case leo_gateway_rpc_handler:get(Key, StartPos, EndPos) of
        {ok,_Metadata, Bin} ->
            {CurPos + ChunkSize, Bin};
        {error, Cause} ->
            {error, Cause}
    end.


%% ---------------------------------------------------------------------
%% API-4
%% ---------------------------------------------------------------------
%% @doc Trim a file which size is modified to Size
-spec(trim(binary(), pos_integer()) ->
             ok | {error, any()}).
trim(Key, Size) ->
    LargeObjectProp = ?env_large_object_properties(),
    ChunkedObjLen = leo_misc:get_value('chunked_obj_len',
                                       LargeObjectProp, ?DEF_LOBJ_CHUNK_OBJ_LEN),
    IsLarge = Size > ChunkedObjLen,
    case leo_gateway_rpc_handler:get(Key) of
        %% TODO Handle Truncate Large Object
        {error, not_found} when IsLarge =:= false ->
            Bin = << 0:(8* Size) >>,
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key,
                                   body = Bin,
                                   dsize = byte_size(Bin)}) of
                {ok,_} ->
                    ok;
                Error ->
                    Error
            end;
        {ok, #?METADATA{cnumber = 0} = _SrcMetadata, SrcObj} when IsLarge =:= false ->
            %% small to small
            %% @todo handle expand case
            << DstObj:Size/binary,_Rest/binary >> = SrcObj,
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key,
                                   body = DstObj,
                                   dsize = byte_size(DstObj)}) of
                {ok,_} ->
                    ok;
                Error ->
                    Error
            end;
        {ok, #?METADATA{cnumber = 0} = _SrcMetadata,_SrcObj} ->
            %% small to large
            %% @todo handle expand case
            ok;
        {ok, #?METADATA{cnumber = CNum} = _SrcMetadata,_} when IsLarge =:= true ->
            %% large to large / @TODO: handle expand case
            End = Size - 1,
            IndexEnd = End div ChunkedObjLen + 1,
            TailSize = End rem ChunkedObjLen + 1,

            case large_obj_partial_trim(Key, IndexEnd, TailSize) of
                ok ->
                    %% Update the metadata based on Size and IndexEnd
                    case leo_gateway_rpc_handler:put(
                           #put_req_params{path = Key,
                                           body = ?BIN_EMPTY,
                                           dsize = Size,
                                           total_chunks = IndexEnd,
                                           csize = ChunkedObjLen,
                                           digest = 0}) of
                        {ok,_} ->
                            %% Remove tail chunks between IndexEnd + 1 to CNum
                            RemovedIdxList = lists:seq(IndexEnd + 1, CNum),
                            large_obj_delete_chunks(Key, RemovedIdxList),
                            ok;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end;
        {ok, #?METADATA{cnumber = _CNum} = _SrcMetadata,_} ->
            %% large to small
            %% Get the first chunk
            IndexBin = list_to_binary(integer_to_list(1)),
            Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
            case leo_gateway_rpc_handler:get(Key_1) of
                {ok,_Metadata, SrcObj} ->
                    %% Trim the data by Size
                    << DstObj:Size/binary,_Rest/binary >> = SrcObj,
                    %% Insert the new object as a small object
                    case leo_gateway_rpc_handler:put(
                           #put_req_params{path = Key,
                                           body = DstObj,
                                           dsize = byte_size(DstObj)}) of
                        {ok,_} ->
                            ok;
                        Error ->
                            Error
                    end;
                Error ->
                    Error
            end
    end.

%% @private
large_obj_partial_trim(Key, Index, Size) ->
    IndexBin = list_to_binary(integer_to_list(Index)),
    Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, IndexBin/binary >>,
    case leo_gateway_rpc_handler:get(Key_1) of
        {ok,_Metadata, SrcObj} ->
            << DstObj:Size/binary,_Rest/binary >> = SrcObj,
            case leo_gateway_rpc_handler:put(
                   #put_req_params{path = Key_1,
                                   body = DstObj,
                                   dsize = byte_size(DstObj)}) of
                {ok,_} ->
                    ok;
                Error ->
                    ?error("large_obj_partial_trim/3",
                           [{key, Key}, {index, Index},
                            {size, Size}, {cause, Error}]),
                    Error
            end;
        Error ->
            Error
    end.

%% @private
large_obj_delete_chunks(_Key, []) ->
    ok;
large_obj_delete_chunks(Key, [Index1|Rest]) ->
    Index2 = list_to_binary(integer_to_list(Index1)),
    Key_1 = << Key/binary, ?DEF_SEPARATOR/binary, Index2/binary >>,
    case leo_gateway_rpc_handler:delete(Key_1) of
        ok ->
            void;
        {error, Cause} ->
            ?error("large_obj_delete_chunks/2",
                   [{key, binary_to_list(Key)},
                    {index, Index1}, {cause, Cause}])
    end,
    large_obj_delete_chunks(Key, Rest).


%% @doc Convert from the file path to the path trailing '/'
%%
-spec(path_to_dir(binary()) ->
             binary()).
path_to_dir(Path) ->
    case binary:last(Path) of
        $/ ->
            Path;
        _ ->
            << Path/binary, "/" >>
    end.


%% @doc Trim the trailing path separator
%%
-spec(path_trim_trailing_sep(binary()) ->
             binary()).
path_trim_trailing_sep(Src) ->
    case binary:last(Src) of
        $/ ->
            binary:part(Src, 0, byte_size(Src) - 1);
        _ ->
            Src
    end.


%% @doc Convert from a relative file path to a absolute one
%%
-spec(path_relative_to_abs(binary()) ->
             binary()).
path_relative_to_abs(P) ->
    path_relative_to_abs(binary:split(P, <<"/">>, [global, trim]), []).

path_relative_to_abs([], []) ->
    <<"/">>;
path_relative_to_abs([], Acc) ->
    filename:join(lists:reverse(Acc));
path_relative_to_abs([<<>>|Rest], Acc) ->
    path_relative_to_abs(Rest, Acc);
path_relative_to_abs([<<".">>|Rest], Acc) ->
    path_relative_to_abs(Rest, Acc);
path_relative_to_abs([<<"..">>|Rest], Acc) ->
    path_relative_to_abs(Rest, tl(Acc));
path_relative_to_abs([Segment|Rest], Acc) ->
    path_relative_to_abs(Rest, [Segment|Acc]).


%% @doc Return true if the specified binary contain _Char, and false otherwise
%%
-spec(binary_is_contained(binary(), char()) ->
             boolean()).
binary_is_contained(<<>>,_Char) ->
    false;
binary_is_contained(<< Char:8,_Rest/binary >>, Char) ->
    true;
binary_is_contained(<< _Other:8, Rest/binary >>, Char) ->
    binary_is_contained(Rest, Char).


%% @doc Return total disk usage on LeoFS in byte
%%
-spec(get_disk_usage() ->
             {ok, {Total::pos_integer(), Free::pos_integer()}}| {error, any()}).
get_disk_usage() ->
    StorageNodes = case leo_redundant_manager_api:get_members() of
                       {ok, Members} ->
                           Nodes = [_N || #member{node = _N,
                                                  state = ?STATE_RUNNING} <- Members],
                           Nodes;
                       {error,_Cause} ->
                           []
                   end,
    get_disk_usage(StorageNodes, {0, 0}).

get_disk_usage([], {Total, Free}) ->
    {ok, {erlang:round(Total * 1024), erlang:round(Free * 1024)}};
get_disk_usage([Node|Rest], {SumTotal, SumFree}) ->
    case rpc:call(Node, leo_storage_api, get_disk_usage, [], 5000) of
        {ok, {Total, Free}} ->
            get_disk_usage(Rest, {SumTotal + Total, SumFree + Free});
        {badrpc, Cause} ->
            {error, Cause};
        Error ->
            Error
    end.
