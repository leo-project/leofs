%%======================================================================
%%
%% Network Lock Manager written in Erlang
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
-module(leo_nlm_proto4_server).

-include("leo_nlm_proto4.hrl").
-include("leo_gateway.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-export([nlmproc4_null_4/2,
         nlmproc4_test_4/3,
         nlmproc4_lock_4/3,
         nlmproc4_cancel_4/3,
         nlmproc4_unlock_4/3,
         nlmproc4_granted_4/3,
         nlmproc4_test_msg_4/3,
         nlmproc4_lock_msg_4/3,
         nlmproc4_cancel_msg_4/3,
         nlmproc4_unlock_msg_4/3,
         nlmproc4_granted_msg_4/3,
         nlmproc4_test_res_4/3,
         nlmproc4_lock_res_4/3,
         nlmproc4_cancel_res_4/3,
         nlmproc4_unlock_res_4/3,
         nlmproc4_granted_res_4/3,
         nlmproc4_share_4/3,
         nlmproc4_unshare_4/3,
         nlmproc4_nm_lock_4/3,
         nlmproc4_free_all_4/3
        ]).

-define(NLM4_GRANTED,               'NLM4_GRANTED').
-define(NLM4_DENIED,                'NLM4_DENIED').
-define(NLM4_DENIED_NOLOCKS,        'NLM4_DENIED_NOLOCKS').
-define(NLM4_BLOCKED,               'NLM4_BLOCKED').
-define(NLM4_DENIED_GRACE_PERIOD,   'NLM4_DENIED_GRACE_PERIOD').
-define(NLM4_DEADLCK,               'NLM4_DEADLCK').
-define(NLM4_ROFS,                  'NLM4_ROFS').
-define(NLM4_STALE_FH,              'NLM4_STALE_FH').
-define(NLM4_FBIG,                  'NLM4_FBIG').
-define(NLM4_FAILED,                'NLM4_FAILED').

-record(nlm_state, {
          handler :: atom()
         }).

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------
%% @doc Called only once from a parent rpc server process to initialize this module
%%      during starting a leo_storage server.
-spec(init(any()) -> {ok, any()}).
init(_Args) ->
    Options = ?env_nlm_options(),
    Handler = leo_misc:get_value('handler', Options, ?DEF_NLM_HANDLER),
    {ok, _} = Handler:start_link([]),
    {ok, #nlm_state{handler = Handler}}.

handle_call(Req, _From, S) ->
    ?debug("handle_call", "req:~p from:~p", [Req, _From]),
    {reply, [], S}.

handle_cast(Req, S) ->
    ?debug("handle_cast", "req:~p", [Req]),
    {reply, [], S}.

handle_info(Req, S) ->
    ?debug("handle_info", "req:~p", [Req]),
    {noreply, S}.

terminate(_Reason, _S) ->
    ok.

%% ---------------------------------------------------------------------
%% API
%% ---------------------------------------------------------------------
%% @doc
nlmproc4_null_4(_Clnt, State) ->
    ?debug("NLM_NULL", "from:~p", [_Clnt]),
    {reply, [], State}.

%% @doc
nlmproc4_test_4({Cookie, IsExclusive, Lock} = TestArgs, Clnt, #nlm_state{handler = Handler} = State) ->
    ?debug("NLM_TEST", "Args:~p, from:~p", [TestArgs, Clnt]),
    {_CallerName, FileHandler, _Owner, _Svid, _Offset, _Length} = Lock,
    NewLock = convert_lock(Lock, IsExclusive),
    TestRet = Handler:test(FileHandler, NewLock),
    LockRet = case TestRet of
                  ok ->
                      {?NLM4_GRANTED};
                  {error, CLock} ->
                      {?NLM4_DENIED, {CLock#lock_record.excl,
                                      CLock#lock_record.uppid,
                                      CLock#lock_record.owner,
                                      CLock#lock_record.start,
                                      CLock#lock_record.len}}
              end,
    {reply, {Cookie, LockRet}, State}.

nlmproc4_lock_4({Cookie, _IsBlock, IsExclusive, Lock, _IsReclaim, _NSMState} = LockArgs, Clnt, #nlm_state{handler = Handler} = State) ->
    ?debug("NLM_LOCK", "Args:~p, from:~p", [LockArgs, Clnt]),
    {_CallerName, FileHandler, _Owner, _Svid, _Offset, _Length} = Lock,
    NewLock = convert_lock(Lock, IsExclusive),
    LockRet = Handler:lock(FileHandler, NewLock),
    Stat = case LockRet of
               ok ->
                   ?NLM4_GRANTED;
               {error, _} ->
                   ?NLM4_DENIED
           end,
    {reply, {Cookie, {Stat}}, State}.

nlmproc4_cancel_4({Cookie, _IsBlock, _IsExclusive, _Lock} = CancArgs, Clnt, State) ->
    ?debug("NLM_CANCEL", "Args:~p, from:~p", [CancArgs, Clnt]),
    {reply, {Cookie, {?NLM4_DENIED}}, State}.

nlmproc4_unlock_4({Cookie, Lock} = UnlockArgs, Clnt, #nlm_state{handler = Handler} = State) ->
    ?debug("NLM_UNLOCK", "Args:~p, from:~p", [UnlockArgs, Clnt]),
    {_CallerName, FileHandler, Owner, _Svid, Offset, Length} = Lock,
    End = Offset + Length - 1,
    Handler:unlock(FileHandler, Owner, Offset, End),
    {reply, {Cookie, {?NLM4_GRANTED}}, State}.

nlmproc4_granted_4({Cookie, _, _} = TestArgs, Clnt, State) ->
    ?debug("NLM_GRANTED", "Args:~p, from:~p", [TestArgs, Clnt]),
    {reply, {Cookie, {?NLM4_DENIED}}, State}.

nlmproc4_test_msg_4(TestArgs, Clnt, State) ->
    ?debug("NLM_TEST_MSG", "Args:~p, from:~p", [TestArgs, Clnt]),
    {noreply, State}.

nlmproc4_lock_msg_4(LockArgs, Clnt, State) ->
    ?debug("NLM_LOCK_MSG", "Args:~p, from:~p", [LockArgs, Clnt]),
    {noreply, State}.

nlmproc4_cancel_msg_4(CancArgs, Clnt, State) ->
    ?debug("NLM_CANCEL_MSG", "Args:~p, from:~p", [CancArgs, Clnt]),
    {noreply, State}.

nlmproc4_unlock_msg_4(UnlockArgs, Clnt, State) ->
    ?debug("NLM_UNLOCK_MSG", "Args:~p, from:~p", [UnlockArgs, Clnt]),
    {noreply, State}.

nlmproc4_granted_msg_4(TestArgs, Clnt, State) ->
    ?debug("NLM_GRANTED_MSG", "Args:~p, from:~p", [TestArgs, Clnt]),
    {noreply, State}.

nlmproc4_test_res_4(TestRes, Clnt, State) ->
    ?debug("NLM_TEST_RES", "Args:~p, from:~p", [TestRes, Clnt]),
    {noreply, State}.

nlmproc4_lock_res_4(Res, Clnt, State) ->
    ?debug("NLM_LOCK_RES", "Args:~p, from:~p", [Res, Clnt]),
    {noreply, State}.

nlmproc4_cancel_res_4(Res, Clnt, State) ->
    ?debug("NLM_CANCEL_RES", "Args:~p, from:~p", [Res, Clnt]),
    {noreply, State}.

nlmproc4_unlock_res_4(Res, Clnt, State) ->
    ?debug("NLM_UNLOCK_RES", "Args:~p, from:~p", [Res, Clnt]),
    {noreply, State}.

nlmproc4_granted_res_4(Res, Clnt, State) ->
    ?debug("NLM_GRANTED_RES", "Args:~p, from:~p", [Res, Clnt]),
    {noreply, State}.

nlmproc4_share_4({Cookie, _, _} = ShareArgs, Clnt, State) ->
    ?debug("NLM_SHARE", "Args:~p, from:~p", [ShareArgs, Clnt]),
    {reply, {Cookie, {?NLM4_DENIED}, 0}, State}.

nlmproc4_unshare_4({Cookie, _, _} = ShareArgs, Clnt, State) ->
    ?debug("NLM_UNSHARE", "Args:~p, from:~p", [ShareArgs, Clnt]),
    {reply, {Cookie, {?NLM4_DENIED}, 0}, State}.

nlmproc4_nm_lock_4({Cookie, _, _, _, _, _} = LockArgs, Clnt, State) ->
    ?debug("NLM_NM_LOCK", "Args:~p, from:~p", [LockArgs, Clnt]),
    {reply, {Cookie,{?NLM4_DENIED}}, State}.

nlmproc4_free_all_4(NotifyArgs, Clnt, State) ->
    ?debug("NLM_FREE_ALL", "Args:~p, from:~p", [NotifyArgs, Clnt]),
    {noreply, State}.

%% ---------------------------------------------------------------------
%% INNER FUNCTIONS
%% ---------------------------------------------------------------------
convert_lock({_, _, Owner, Uppid, Offset, Length}, Excl) ->
    End = case Length of
              0 ->
                  -1;
              _ ->
                  Offset + Length - 1
          end,
    #lock_record{
       start = Offset,
       till = End,
       len = Length,
       owner = Owner,
       uppid = Uppid,
       excl = Excl}.

%% ---------------------------------------------------------------------
%% UNIT TESTS
%% ---------------------------------------------------------------------
-ifdef(EUNIT).
convert_lock_test() ->
    ?debugMsg("===== Testing Convert Lock ====="),
    Lock = {
      <<"freebsd102">>, %% CallerName
      <<159,64,92,200,58,233,78,51,175,128,47,36,35,144,134,200>>,
      %% FileHandler
      <<"6350@freebsd102">>, %% Owner
      6350, %% Svid
      1, %% Offset
      10 %% Length
     },
    #lock_record{start = 1,
                 till = 10,
                 len = 10,
                 owner = <<"6350@freebsd102">>,
                 uppid = 6350,
                 excl = true
                } = convert_lock(Lock, true),

    ?debugMsg("===== Testing Convert Lock with 0 Length (Whole File) ====="),
    Lock2 = {<<"freebsd102">>, %% CallerName
             <<159,64,92,200,58,233,78,51,175,128,47,36,35,144,134,200>>,
             %% FileHandler
             <<"6350@freebsd102">>, %% Owner
             6350, %% Svid
             1, %% Offset
             0  %% Length
            },
    #lock_record{start = 1,
                 till = -1,
                 len = 0,
                 owner = <<"6350@freebsd102">>,
                 uppid = 6350,
                 excl = false} = convert_lock(Lock2, false).
-endif.
