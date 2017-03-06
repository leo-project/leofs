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
-module(leo_nfs_readdir_state_ets).

-behaviour(leo_nfs_readdir_state_behaviour).
-behaviour(gen_server).

-include("leo_gateway.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_commons/include/leo_commons.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
-export([info/0, start_link/1]).
-export([add_readdir_entry/2, get_readdir_entry/1, del_readdir_entry/1]).

-record(state, {
          scan_interval :: integer(),
          entry_ttl     :: integer()
         }).


-define(LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, readdir_entry_ets_tbl).

info() ->
    ets:info(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL).


start_link(Params) ->
    ets:new(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, [set, named_table, public]),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Params], []).


%% @doc
add_readdir_entry(CookieVerf, ReadDirEntry) ->
    Now = leo_date:now(),
    gen_server:call(?MODULE, scan),
    ets:insert(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL,
               {CookieVerf, {Now, ReadDirEntry}}).


%% @doc
get_readdir_entry(CookieVerf) ->
    Ret = ets:lookup(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, CookieVerf),
    case Ret of
        [] ->
            not_found;
        [{_, {_, ReadDirEntry}}|_] ->
            Now = leo_date:now(),
            ets:insert(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, {CookieVerf, {Now, ReadDirEntry}}),
            {ok, ReadDirEntry}
    end.


%% @doc
del_readdir_entry(CookieVerf) ->
    ets:delete(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, CookieVerf).

%% ---------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%% ---------------------------------------------------------------------
init([Args]) ->
    ScanInterval = leo_misc:get_value('nfsd_readdir_scan_int', Args, ?DEF_NFSD_READDIR_SCAN_INT) * 1000,
    EntryTTL = leo_misc:get_value('nfsd_readdir_entry_ttl', Args, ?DEF_NFSD_READDIR_ENTRY_TTL),
    ?debug("init/1", "Scan Int: ~p, TTL: ~p", [ScanInterval, EntryTTL]),
    erlang:send_after(ScanInterval, self(), scan),
    {ok, #state{scan_interval = ScanInterval,
                entry_ttl = EntryTTL}}.


handle_call(scan, _, #state{entry_ttl = EntryTTL} = S) ->
    cleanup(EntryTTL),
    {reply, ok, S}.


handle_cast(_, S) ->
    {noreply, S}.


handle_info(scan, #state{scan_interval = ScanInterval,
                         entry_ttl = EntryTTL} = S) ->
    cleanup(EntryTTL),
    erlang:send_after(ScanInterval, self(), scan),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ---------------------------------------------------------------------
%% PRIVATE METHODS 
%% ---------------------------------------------------------------------
cleanup(EntryTTL) ->
    Now = leo_date:now(),
    Limit = Now - EntryTTL,
    MatchSpec = ets:fun2ms(fun({Cookie, {Time, _}}) when Time < Limit ->
                                   true
                           end),
    Cnt = ets:select_delete(?LEO_GW_NFS_READDIR_ENTRY_ETS_TBL, MatchSpec),
    ?debug("cleanup/1", "Cleaned up: ~p", [Cnt]),
    Cnt.
