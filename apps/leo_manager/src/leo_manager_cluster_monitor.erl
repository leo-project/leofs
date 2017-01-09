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
-module(leo_manager_cluster_monitor).

-behaviour(gen_server).

-include("leo_manager.hrl").
-include_lib("leo_logger/include/leo_logger.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-undef(CRLF).
-include_lib("leo_rpc/include/leo_rpc.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([start_link/0,
         stop/0]).

-export([register/4, register/7, register/8,
         demonitor/1,
         get_remote_node_proc/0,
         get_remote_node_proc/2,
         get_server_node_alias/1,
         sync_ring/1
        ]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-undef(DEF_TIMEOUT).
-define(DEF_TIMEOUT, 30000).

-record(registration, {pid           :: pid(),
                       node          :: atom(),
                       type          :: atom(),
                       times         :: atom(),
                       level_1 = []  :: string(),
                       level_2 = []  :: string(),
                       num_of_vnodes = ?DEF_NUMBER_OF_VNODES :: pos_integer(),
                       rpc_port = ?DEF_LISTEN_PORT :: pos_integer()
                      }).

%%--------------------------------------------------------------------
%% API
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:call(?MODULE, stop, ?DEF_TIMEOUT).


%% @doc Register gateway and storage pid in monitor.
%%
-spec(register(atom(), pid(), atom(), atom()) ->
             ok).
register(RequestedTimes, Pid, Node, TypeOfNode) ->
    RegistrationInfo = #registration{pid   = Pid,
                                     node  = Node,
                                     type  = TypeOfNode,
                                     times = RequestedTimes},
    gen_server:call(?MODULE, {register, RegistrationInfo}, ?DEF_TIMEOUT).

-spec(register(atom(), pid(), atom(), atom(),
               string(), string(), pos_integer()) ->
             ok).
register(RequestedTimes, Pid, Node, TypeOfNode, L1Id, L2Id, NumOfVNodes) ->
    register(RequestedTimes, Pid, Node, TypeOfNode, L1Id, L2Id, NumOfVNodes, ?DEF_LISTEN_PORT).

-spec(register(atom(), pid(), atom(), atom(),
               string(), string(), pos_integer(), pos_integer()) ->
             ok).
register(RequestedTimes, Pid, Node, TypeOfNode, L1Id, L2Id, NumOfVNodes, RPCPort) ->
    RegistrationInfo = #registration{pid   = Pid,
                                     node  = Node,
                                     type  = TypeOfNode,
                                     times = RequestedTimes,
                                     level_1 = L1Id,
                                     level_2 = L2Id,
                                     num_of_vnodes = NumOfVNodes,
                                     rpc_port = RPCPort
                                    },
    gen_server:call(?MODULE, {register, RegistrationInfo}, ?DEF_TIMEOUT).


%% @doc Demonitor pid from monitor.
%%
-spec(demonitor(atom()) ->
             ok | undefined).
demonitor(Node) ->
    gen_server:call(?MODULE, {demonitor, Node}, ?DEF_TIMEOUT).


%% @doc Retrieve pid of remote-nodes.
%%
-spec(get_remote_node_proc() -> ok ).
get_remote_node_proc() ->
    gen_server:cast(?MODULE, get_remote_node_proc).

%% @doc Retrieve pid of remote-nodes.
%%
-spec(get_remote_node_proc(ServerType, Node) ->
             ok when ServerType::atom(),
                     Node::atom()).
get_remote_node_proc(ServerType, Node) ->
    gen_server:cast(?MODULE, {get_remote_node_proc, ServerType, Node}).


%% @doc Retrieve node-alias.
%%
-spec(get_server_node_alias(Node::atom()) -> {ok, tuple()}).
get_server_node_alias(Node) ->
    NewNode = case is_atom(Node) of
                  true -> Node;
                  _    -> list_to_atom(Node)
              end,
    gen_server:call(?MODULE, {get_server_node_alias, NewNode}, ?DEF_TIMEOUT).

%% @doc Syncronize RING between manager and storage/gateway
%%
-spec(sync_ring(Node) ->
             ok when Node::atom()).
sync_ring(Node) ->
    gen_server:cast(?MODULE, {sync_ring, Node}).


%%--------------------------------------------------------------------
%% GEN_SERVER CALLBACKS
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([]) ->
    {ok, {_Refs = [],
          _Htbl = [],
          _Pids = []}}.


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};


handle_call({register, RegistrationInfo}, _From, {Refs, Htbl, Pids} = Arg) ->
    ?debug("handle_call - register", "requested-times:~w, node:~w",
           [RegistrationInfo#registration.times,
            RegistrationInfo#registration.node]),
    #registration{pid   = Pid,
                  node  = Node,
                  type  = TypeOfNode} = RegistrationInfo,

    case is_exists_proc(Htbl, Pid, Node) of
        true ->
            Ret = register_fun_1(RegistrationInfo),
            {reply, Ret, Arg};
        false ->
            Htbl_1 = case find_by_node_alias(Htbl, Node) of
                         undefined ->
                             Htbl;
                         {Pid_1, MonitorRef_1} ->
                             erlang:demonitor(MonitorRef_1),
                             delete_by_pid(Htbl, Pid_1)
                     end,

            case register_fun_1(RegistrationInfo) of
                ok ->
                    MonitorRef = erlang:monitor(process, Pid),
                    ProcInfo   = {Pid, {atom_to_list(Node), Node, TypeOfNode, MonitorRef}},
                    {reply, ok, {_Refs = [MonitorRef | Refs  ],
                                 _Htbl = [ProcInfo   | Htbl_1],
                                 _Pids = Pids}};
                Error ->
                    {reply, Error, Arg}
            end
    end;

handle_call({demonitor, Node}, _From, {MonitorRefs, Htbl, Pids} = Arg) ->
    case find_by_node_alias(Htbl, Node) of
        undefined ->
            {reply, undefined, Arg};
        {Pid, MonitorRef} ->
            erlang:demonitor(MonitorRef),
            NewHtbl = delete_by_pid(Htbl, Pid),

            {reply, ok, {_MonitorRefs = lists:delete(MonitorRef, MonitorRefs),
                         NewHtbl,
                         _Pids = lists:delete(Pid, Pids)}}
    end;


handle_call({get_server_node_alias, Node}, _From, {Refs, Htbl, Pids}) ->
    Reply = lists:foldl(
              fun(X, N) ->
                      {_, {_, NodeAlias, _TypeOfNode, _MonitorRef}} = X,
                      case Node of
                          NodeAlias ->
                              NodeAlias;
                          _ ->
                              N
                      end
              end, undefined, Htbl),
    {reply, Reply, {Refs, Htbl, Pids}}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
handle_cast(get_remote_node_proc, State) ->
    ok = get_remote_node_proc_fun(),
    {noreply, State};

handle_cast({get_remote_node_proc, ServerType, Node}, State) ->
    _ = get_remote_node_proc_fun(ServerType, Node),
    {noreply, State};

handle_cast({sync_ring, Node}, State) ->
    case sync_ring_fun(Node) of
        ok ->
            ok;
        _Other ->
            case leo_misc:node_existence(Node) of
                true ->
                    timer:apply_after(
                      ?APPLY_AFTER_TIME, ?MODULE, sync_ring, [Node]);
                false ->
                    void
            end
    end,
    {noreply, State};

handle_cast(_Message, State) ->
    {noreply, State}.


%% Function: handle_info(Info, State) -> {noreply, State}          |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
handle_info({'DOWN', MonitorRef, _Type, Pid, _Info}, {MonitorRefs, Htbl, Pids}) ->
    timer:sleep(random:uniform(500)),
    NewHtbl =
        case find_by_pid(Htbl, Pid) of
            undefined ->
                Htbl;
            {_, Node, TypeOfNode, _} ->
                ?error("handle_call - DOWN", [{node, Node}]),

                case TypeOfNode of
                    ?WORKER_NODE ->
                        catch leo_manager_mnesia:update_gateway_node(
                                #node_state{node    = Node,
                                            state   = ?STATE_STOP,
                                            when_is = ?CURRENT_TIME});
                    ?PERSISTENT_NODE ->
                        case catch leo_redundant_manager_api:get_member_by_node(Node) of
                            {ok, #member{state = State}} ->
                                case update_node_state(down, State, Node) of
                                    delete ->
                                        ok = leo_cluster_tbl_member:delete(Node),
                                        case catch leo_manager_mnesia:get_storage_node_by_name(Node) of
                                            {ok, NodeInfo} ->
                                                leo_manager_mnesia:delete_storage_node(NodeInfo);
                                            _Other ->
                                                void
                                        end;
                                    _Other ->
                                        void
                                end;
                            _Error ->
                                void
                        end
                end,
                delete_by_pid(Htbl, Pid)
        end,

    erlang:demonitor(MonitorRef),
    {noreply, {_MonitorRefs = lists:delete(MonitorRef, MonitorRefs),
               NewHtbl,
               _Pids = lists:delete(Pid, Pids)}};

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
%% Internal Functions
%%--------------------------------------------------------------------
%% @doc Modify node state.
%%
-spec(update_node_state(start|down, node_state()|not_found, atom()) ->
             ok | delete | {error, any()}).
update_node_state(start, ?STATE_ATTACHED, _Node) -> ok;
update_node_state(start, ?STATE_DETACHED, _Node) -> ok;
update_node_state(start, ?STATE_SUSPEND,   Node) -> update_node_state_1(?STATE_SUSPEND, Node, leo_date:clock());
update_node_state(start, ?STATE_RUNNING,  _Node) -> ok;
update_node_state(start, ?STATE_STOP,      Node) -> update_node_state_1(?STATE_RUNNING, Node);
update_node_state(start, ?STATE_RESTARTED,_Node) -> ok;
update_node_state(start, not_found,        Node) -> update_node_state_1(?STATE_ATTACHED,  Node, leo_date:clock());

update_node_state(down,  ?STATE_ATTACHED, _Node) -> delete;
update_node_state(down,  ?STATE_DETACHED, _Node) -> ok;
update_node_state(down,  ?STATE_SUSPEND,  _Node) -> ok;
update_node_state(down,  ?STATE_RUNNING,   Node) -> update_node_state_1(?STATE_STOP, Node);
update_node_state(down,  ?STATE_STOP,     _Node) -> ok;
update_node_state(down,  ?STATE_RESTARTED, Node) -> update_node_state_1(?STATE_STOP, Node);
update_node_state(down,  not_found,       _Node) -> ok.

update_node_state_1(State, Node) ->
    update_node_state_1(State, Node, -1).
update_node_state_1(State, Node, Clock) ->
    case leo_manager_mnesia:update_storage_node_status(
           update, #node_state{node = Node,
                               ring_hash_new = [],
                               ring_hash_old = [],
                               when_is       = ?CURRENT_TIME}) of
        ok ->
            case leo_redundant_manager_api:update_member_by_node(Node, Clock, State) of
                ok ->
                    leo_manager_api:distribute_members(undefined);
                Error ->
                    Error
            end;
        Error ->
            Error
    end.


%% @doc Register pid of remote-nodes in this monitor.
%%
-spec(get_remote_node_proc_fun() ->
             ok).
get_remote_node_proc_fun() ->
    Nodes_0 = case leo_manager_mnesia:get_gateway_nodes_all() of
                  {ok, R1} ->
                      [{gateway,_N1,_S1} || #node_state{node  = _N1,
                                                        state = _S1} <- R1];
                  _ ->
                      []
              end,
    Nodes_1 = case leo_redundant_manager_api:get_members() of
                  {ok, R2} ->
                      [{storage,_N2,_S2} || #member{node = _N2,
                                                    state = _S2} <- R2];
                  _Error ->
                      []
              end,
    lists:foreach(
      fun({_Type, _Node, ?STATE_DETACHED}) -> void;
         ({_Type, _Node, ?STATE_SUSPEND})  -> void;
         ({_Type, _Node, ?STATE_STOP})     -> void;
         ({ Type,  Node, _}) ->
              spawn(
                fun() ->
                        case get_remote_node_proc_fun(Type, Node) of
                            ok ->
                                void;
                            {error,_Cause} ->
                                timer:apply_after(
                                  ?APPLY_AFTER_TIME, ?MODULE,
                                  get_remote_node_proc, [Type, Node])
                        end
                end)
      end,  Nodes_0 ++ Nodes_1),
    ok.

%% @private
get_remote_node_proc_fun(storage, Node) ->
    case leo_misc:node_existence(Node) of
        true ->
            Mod = leo_storage_api,
            case rpc:call(Node, Mod, register_in_monitor, [again], 5000) of
                ok ->
                    ok;
                {_, Cause} ->
                    {error, Cause};
                timeout = Cause ->
                    {error, Cause}
            end;
        false ->
            {error, 'not_connected'}
    end;

get_remote_node_proc_fun(gateway, Node) ->
    case leo_misc:node_existence(Node) of
        true ->
            Mod = leo_gateway_api,
            case rpc:call(Node, Mod, register_in_monitor, [again], ?DEF_TIMEOUT) of
                ok ->
                    ok;
                {_, Cause} ->
                    {error, Cause};
                timeout = Cause ->
                    {error, Cause}
            end;
        false ->
            {error, 'not_connected'}
    end.


%% @doc Sync RING between manager and storage/gateway node
sync_ring_fun(Node) ->
    case catch leo_manager_api:synchronize(?CHECKSUM_MEMBER, Node) of
        ok ->
            case catch leo_manager_api:synchronize(?CHECKSUM_RING, Node) of
                ok ->
                    case catch leo_manager_api:recover(?RECOVER_RING, Node, true) of
                        ok ->
                            ok;
                        Other ->
                            Other
                    end;
                Other ->
                    Other
            end;
        Other ->
            Other
    end.


%% @doc Returns true if exists a process, false otherwise
%%
-spec(is_exists_proc(list(), pid(), atom()) ->
             boolean()).
is_exists_proc(ProcList, Pid, Node) ->
    lists:foldl(fun({P, {_,N,_,_}},_) when Node == N,
                                           Pid  == P ->
                        true;
                   ({_, {_,_,_,_}},SoFar) ->
                        SoFar
                end, false, ProcList).


%% @doc Retrieve a process by the node-alias
%%
-spec(find_by_node_alias(list(), atom()) ->
             undefined | {pid(), reference()}).
find_by_node_alias(ProcList, Node) ->
    lists:foldl(fun({ Pid, {_, N,_, MonitorRef}},_S) when Node == N ->
                        {Pid, MonitorRef};
                   ({_Pid, {_,_N,_,_MonitorRef}}, S) ->
                        S
                end, undefined, ProcList).


%% @doc Returns a process by the pid
%%
-spec(find_by_pid(list(), pid()) ->
             undefined | tuple()).
find_by_pid(ProcList, Pid0) ->
    lists:foldl(fun({Pid1, ProcInfo}, undefined) when Pid0 == Pid1 ->
                        ProcInfo;
                   (_, Acc) ->
                        Acc
                end, undefined, ProcList).


%% @doc Remove a process by the pid.
%%
-spec(delete_by_pid(list(), pid()) ->
             list()).
delete_by_pid(ProcList, Pid0) ->
    lists:foldl(fun({Pid1, _}, Acc) when Pid0 == Pid1 ->
                        Acc;
                   (ProcInfo,  Acc) ->
                        [ProcInfo|Acc]
                end, [], ProcList).


%% @doc Register a remote-node's process into the monitor
%%
-spec(register_fun_1(#registration{}) ->
             ok | {error, any()}).
register_fun_1(#registration{node = Node,
                             type = ?WORKER_NODE}) ->
    case leo_manager_mnesia:get_gateway_node_by_name(Node) of
        {ok, #node_state{state = ?STATE_RUNNING}} ->
            ok;
        {error, Cause} ->
            ?error("register_fun_1/2", [{cause, Cause}]),
            {error, Cause};
        _Other ->
            case rpc:call(Node, leo_redundant_manager_api,
                          checksum, [?CHECKSUM_RING], ?DEF_TIMEOUT) of
                {ok, {Chksum0, Chksum1}} ->
                    leo_manager_mnesia:update_gateway_node(
                      #node_state{node    = Node,
                                  state   = ?STATE_RUNNING,
                                  ring_hash_new = leo_hex:integer_to_hex(Chksum0, 8),
                                  ring_hash_old = leo_hex:integer_to_hex(Chksum1, 8),
                                  when_is = ?CURRENT_TIME});
                Error ->
                    Error
            end
    end;

register_fun_1(#registration{node = Node,
                             type = ?PERSISTENT_NODE} = RegistrationInfo) ->
    Ret = leo_redundant_manager_api:get_member_by_node(Node),
    register_fun_2(Ret, RegistrationInfo);
register_fun_1(_) ->
    ok.


-spec(register_fun_2({ok, #member{}} | not_found| {error, any()}, #registration{}) ->
             ok | {error, any()}).
register_fun_2({ok, #member{state = State}},
               #registration{node = Node,
                             type = ?PERSISTENT_NODE}) when State == ?STATE_RUNNING;
                                                            State == ?STATE_STOP ->
    %% synchronize member and ring
    case sync_ring_fun(Node) of
        ok ->
            ok;
        _Error ->
            timer:apply_after(
              ?APPLY_AFTER_TIME, ?MODULE, sync_ring, [Node])
    end,
    update_node_state(start, State, Node);

register_fun_2({ok, #member{state = ?STATE_DETACHED}},
               #registration{node = Node,
                             type = ?PERSISTENT_NODE,
                             level_1 = L1,
                             level_2 = L2,
                             num_of_vnodes = NumOfVNodes}) ->
    case leo_manager_api:attach(Node, L1, L2, NumOfVNodes) of
        ok ->
            update_node_state_1(?STATE_RESTARTED, Node);
        {error, Cause} ->
            ?error("register_fun_2/2",
                   [{node, Node}, {cause, Cause}]),
            {error, Cause}
    end;

register_fun_2({ok, #member{state = State}},
               #registration{node = Node,
                             type = ?PERSISTENT_NODE}) ->
    update_node_state(start, State, Node);

register_fun_2({error, not_found}, #registration{node = Node,
                                                 type = ?PERSISTENT_NODE,
                                                 level_1 = L1,
                                                 level_2 = L2,
                                                 num_of_vnodes = NumOfVNodes,
                                                 rpc_port = RPCPort}) ->
    case leo_manager_api:attach(Node, L1, L2, NumOfVNodes, RPCPort) of
        ok ->
            case update_node_state(start, not_found, Node) of
                ok ->
                    ok;
                {error, Cause} ->
                    ?error("register_fun_2/2",
                           [{node, Node}, {cause, Cause}]),
                    {error, Cause}
            end;
        {error, Cause} ->
            ?error("register_fun_2/2",
                   [{node, Node}, {cause, Cause}]),
            {error, Cause}
    end;

register_fun_2({error, Cause}, #registration{node = Node,
                                             type = ?PERSISTENT_NODE}) ->
    ?error("register_fun_2/2",
           [{node, Node}, {cause, Cause}]),
    {error, Cause}.
