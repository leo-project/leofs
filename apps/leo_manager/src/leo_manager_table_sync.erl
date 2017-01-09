%%======================================================================
%%
%% Leo Manager
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
-module(leo_manager_table_sync).

-behaviour(gen_server).

-include("leo_manager.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("leo_s3_libs/include/leo_s3_auth.hrl").
-include_lib("leo_s3_libs/include/leo_s3_bucket.hrl").
-include_lib("leo_s3_libs/include/leo_s3_libs.hrl").
-include_lib("leo_s3_libs/include/leo_s3_user.hrl").
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/0,
         stop/0]).
-export([sync/0,
         force_sync/0
        ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
	       terminate/2,
         code_change/3]).

-undef(DEF_TIMEOUT).
-ifdef(TEST).
-define(DEF_INTERVAL,  1000).
-define(DEF_TIMEOUT,   1000).
-else.
-define(DEF_INTERVAL,  timer:seconds(30)).
-define(DEF_TIMEOUT,   30000).
-endif.

-record(sync_state, {interval  = ?DEF_INTERVAL :: integer(),
                     timestamp = 0 :: integer()
                    }).


%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [?DEF_INTERVAL], []).

stop() ->
    gen_server:call(?MODULE, stop, 30000).


-spec(sync() -> ok | {error, any()}).
sync() ->
    gen_server:cast(?MODULE, sync).


-spec(force_sync() -> ok | {error, any()}).
force_sync() ->
    gen_server:call(?MODULE, force_sync).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([Interval]) ->
    defer_sync(Interval),
    {ok, #sync_state{interval  = Interval,
                     timestamp = 0}}.


handle_call(stop,_From,State) ->
    {stop, normal, ok, State};

handle_call(force_sync,_From, State) ->
    ok = sync_1(),
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast(sync, State) ->
    case catch maybe_sync(State) of
        {'EXIT', _Reason} ->
            {noreply, State};
        NewState ->
            {noreply, NewState}
    end.

%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
handle_info(_Info, State) ->
    {noreply, State}.

%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
terminate(_Reason, _State) ->
    ok.

%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% INTERNAL FUNCTIONS
%%--------------------------------------------------------------------
%% @doc Heatbeat
%% @private
-spec(maybe_sync(#sync_state{}) ->
             #sync_state{}).
maybe_sync(#sync_state{interval  = Interval,
                       timestamp = Timestamp} = State) ->
    ThisTime = leo_date:now() * 1000,

    case ((ThisTime - Timestamp) < Interval) of
        true ->
            void;
        false ->
            sync_1()
    end,

    defer_sync(Interval),
    State#sync_state{timestamp = leo_date:now() * 1000}.


%% @doc Heartbeat
%% @private
-spec(defer_sync(integer()) ->
             ok | any()).
defer_sync(Time) ->
    catch timer:apply_after(Time, ?MODULE, sync, []).


%% @doc Synchronize remote-cluster's status/configurations
%% @private
sync_1() ->
    case leo_mdcr_tbl_cluster_mgr:all() of
        {ok, Managers} ->
            ok = exec(Managers);
        not_found ->
            void;
        {error, Cause} ->
            error_logger:error_msg("~p,~p,~p,~p~n",
                                   [{module, ?MODULE_STRING},
                                    {function, "sync/1"},
                                    {line, ?LINE}, {body, Cause}])
    end,
    ok.


%% @doc Retrieve records of users and buckets,
%%      and then if find inconsistent records, fix them
%% @private
-spec(exec(list(#cluster_manager{})) ->
             ok | {error, any()}).
exec([]) ->
    ok;
exec([#cluster_manager{node = Node,
                       cluster_id = _ClusterId}|Rest]) ->
    %% Synchronize s3-related tables:
    %% [ leo_s3_credentials,
    %%   leo_s3_buckets,
    %%   leo_s3_users,
    %%   leo_s3_user_credential ]
    case catch leo_s3_libs:get_checksums() of
        {'EXIT',Cause} ->
            {error, Cause};
        {ok, S3TblsChecksum} ->
            case compare_with_remote_node(Node, S3TblsChecksum) of
                ok ->
                    ok;
                _Error ->
                    exec(Rest)
            end
    end.


%% @doc Compare s3-related tables of checksum with a remote-node
%% @private
compare_with_remote_node(Node,
                         #s3_tbls_checksum{auth       = L_C1,
                                           bucket     = L_C2,
                                           user       = L_C3,
                                           credential = L_C4
                                          } = _S3TblsChecksum_1) ->
    case catch leo_rpc:call(Node, leo_s3_libs, get_checksums, []) of
        {ok, #s3_tbls_checksum{auth       = R_C1,
                               bucket     = R_C2,
                               user       = R_C3,
                               credential = R_C4
                              } = _S3TblsChecksum_2} ->
            Ret = [{auth,       (L_C1 == R_C1)},
                   {bucket,     (L_C2 == R_C2)},
                   {user,       (L_C3 == R_C3)},
                   {credential, (L_C4 == R_C4)}],
            compare_with_remote_node_1(Ret, Node);
        _Error ->
            _Error
    end.

%% @private
compare_with_remote_node_1([],_Node) ->
    ok;
compare_with_remote_node_1([{auth, false}|Rest], Node) ->
    Mod = leo_s3_auth,
    ok = compare_with_remote_node_2(Node, Mod),
    compare_with_remote_node_1(Rest, Node);
compare_with_remote_node_1([{bucket, false}|Rest], Node) ->
    Mod = leo_s3_bucket,
    ok = compare_with_remote_node_2(Node, Mod),
    compare_with_remote_node_1(Rest, Node);
compare_with_remote_node_1([{user, false}|Rest], Node) ->
    Mod = leo_s3_user,
    ok = compare_with_remote_node_2(Node, Mod),
    compare_with_remote_node_1(Rest, Node);
compare_with_remote_node_1([{credential, false}|Rest], Node) ->
    Mod = leo_s3_user_credential,
    ok = compare_with_remote_node_2(Node, Mod),
    compare_with_remote_node_1(Rest, Node);
compare_with_remote_node_1([_|Rest], Node) ->
    compare_with_remote_node_1(Rest, Node).

%% @private
compare_with_remote_node_2(Node, Mod) ->
    %% Retrieve local records
    case Mod:find_all() of
        {ok, RetL_Local} ->
            %% Retrieve remote records
            case leo_rpc:call(Node, Mod, find_all, []) of
                {ok, RetL_Remote} ->
                    compare_with_remote_node_3(RetL_Remote, RetL_Local);
                not_found ->
                    fix_remote_table(RetL_Local, Node);
                Error ->
                    Error
            end;
        _Error ->
            void
    end,
    ok.

%% @private
compare_with_remote_node_3([],_LocalRecs) ->
    ok;
compare_with_remote_node_3([Value|Rest], LocalRecs) ->
    ok = compare_with_remote_node_4(Value, LocalRecs),
    compare_with_remote_node_3(Rest, LocalRecs).

%% @private
compare_with_remote_node_4(#credential{} = Credential, []) ->
    leo_s3_auth:put(Credential);
compare_with_remote_node_4(#?BUCKET{} = Bucket, []) ->
    leo_s3_bucket:put(Bucket);
compare_with_remote_node_4(#?S3_USER{} = User, []) ->
    leo_s3_user:put(User);
compare_with_remote_node_4(#user_credential{} = UserCredential, []) ->
    leo_s3_user_credential:put(UserCredential);
compare_with_remote_node_4(_Other, []) ->
    ok;

compare_with_remote_node_4(#credential{access_key_id = Id} = Credential_1,
                           [#credential{access_key_id = Id} = Credential_2|Rest]) ->
    case (Credential_1 == Credential_2) of
        true ->
            compare_with_remote_node_4(Credential_1, Rest);
        false ->
            leo_s3_auth:put(Credential_1)
    end;
compare_with_remote_node_4(#?BUCKET{name = Name,
                                    last_modified_at = Upd_1} = Bucket_1,
                           [#?BUCKET{name = Name,
                                     last_modified_at = Upd_2} = Bucket_2|Rest]) ->
    case (Bucket_1 == Bucket_2) of
        true ->
            compare_with_remote_node_4(Bucket_1, Rest);
        false when Upd_1 > Upd_2 ->
            leo_s3_bucket:put(Bucket_1);
        false ->
            ok
    end;
compare_with_remote_node_4(#?S3_USER{id = Id,
                                     updated_at = Upd_1} = User_1,
                           [#?S3_USER{id = Id,
                                      updated_at = Upd_2} = User_2|Rest]) ->
    case (User_1 == User_2) of
        true ->
            compare_with_remote_node_4(User_1, Rest);
        false when Upd_1 > Upd_2 ->
            leo_s3_user:put(User_1);
        false ->
            ok
    end;
compare_with_remote_node_4(#user_credential{user_id = UserId} = UserCredential_1,
                           [#user_credential{user_id = UserId} = UserCredential_2|Rest]) ->
    case (UserCredential_1 == UserCredential_2) of
        true ->
            compare_with_remote_node_4(UserCredential_1, Rest);
        false ->
            leo_s3_user_credential:put(UserCredential_1)
    end;
compare_with_remote_node_4(Val, [_|Rest]) ->
    compare_with_remote_node_4(Val, Rest).


%% @doc Fix records of a remote-cluster's table
%% @private
fix_remote_table([#credential{}|_] = L, Node) ->
    catch leo_rpc:call(Node, leo_s3_auth, bulk_put, [L]),
    ok;
fix_remote_table([#?BUCKET{}|_] = L, Node) ->
    catch leo_rpc:call(Node, leo_s3_bucket, bulk_put, [L]),
    ok;
fix_remote_table([#?S3_USER{}|_] = L, Node) ->
    catch leo_rpc:call(Node, leo_s3_user, bulk_put, [L]),
    ok;
fix_remote_table([#user_credential{}|_] = L, Node) ->
    catch leo_rpc:call(Node, leo_s3_user_credential, bulk_put, [L]),
    ok;
fix_remote_table([_|_],_Node) ->
    ok.
