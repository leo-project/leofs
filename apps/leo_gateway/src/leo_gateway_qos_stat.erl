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
%% -------------------------------------------------------------------
%% Leo Gateway - API
%% @doc
%% @end
%%====================================================================
-module(leo_gateway_qos_stat).

-behaviour(gen_server).

-include("leo_gateway.hrl").
-include_lib("savanna_commons/include/savanna_commons.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([start_link/2, stop/0]).
-export([notify/4]).
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(METRIC_BUCKET_H_CONS, [{?HISTOGRAM_CONS_SAMPLE, 3000}]).
-define(METRIC_BUCKET_COLS, [
                             %% req counters
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_1,
                                         type = ?COL_TYPE_COUNTER},
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_2,
                                         type = ?COL_TYPE_COUNTER},
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_3,
                                         type = ?COL_TYPE_COUNTER},
                             %% total object length
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_4,
                                         type = ?COL_TYPE_COUNTER},
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_5,
                                         type = ?COL_TYPE_COUNTER},
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_6,
                                         type = ?COL_TYPE_COUNTER},
                             %% histogram object length
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_7,
                                         type = ?COL_TYPE_H_UNIFORM,
                                         constraint = ?METRIC_BUCKET_H_CONS
                                        },
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_8,
                                         type = ?COL_TYPE_H_UNIFORM,
                                         constraint = ?METRIC_BUCKET_H_CONS
                                        },
                             #?SV_COLUMN{schema_name = ?QOS_METRIC_BUCKET_SCHEMA,
                                         name = ?QOS_METRIC_BUCKET_COL_9,
                                         type = ?COL_TYPE_H_UNIFORM,
                                         constraint = ?METRIC_BUCKET_H_CONS
                                        }
                            ]).
-define(METRIC_BUCKET_WINDOW, 20).

-define(DEF_MAX_TIMES_OF_NOTICE, 3).
-define(ERROR_COULD_NOT_CONNECT_SVM,
        "Could not connect sv_managers").
-define(ERROR_COULD_NOT_SYNC_TBL,
        "Could not synchronize tables").

-record(state, {managers = [] :: list(),
                is_enabled = false :: boolean()}).


%% ===================================================================
%% API
%% ===================================================================
%% @doc Launch savanna-agent
%%      and create a schema and columns
%%
-spec(start_link(list(), boolean()) ->
             ok | {error, any()}).
start_link(Managers, IsEnabled) ->
    gen_server:start_link(?MODULE, [Managers, IsEnabled], []).


%% @doc Stop this server
%%
-spec(stop() -> ok).
stop() ->
    gen_server:call(?MODULE, stop, ?DEF_TIMEOUT).


%% @doc Notify a message to savanna_agent
%%
-spec(notify(atom(), atom(), atom(), any())->
             ok).
notify(Schema, MetricGroup, Col, Val) ->
    gen_server:cast(?MODULE, {notify, Schema, MetricGroup, Col, Val}).


%%====================================================================
%% GEN_SERVER CALLBACKS
%%====================================================================
%% Function: init(Args) -> {ok, State}          |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
init([SVManagers, IsEnabled]) ->
    %% create a schema for bucket-metrics
    case IsEnabled of
        true ->
            case svc_tbl_schema:get(?QOS_METRIC_BUCKET_SCHEMA) of
                not_found ->
                    create_schema(SVManagers);
                _ ->
                    void
            end;
        false ->
            void
    end,
    {ok, #state{managers = SVManagers,
                is_enabled = IsEnabled}}.

handle_call(_Msg,_From, State) ->
    {reply, ok, State}.


%% Function: handle_cast(Msg, State) -> {noreply, State}          |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast message
handle_cast({notify,_,_,_,_}, #state{is_enabled = false} = State) ->
    {noreply, State};
handle_cast({notify, Schema, MetricGroup, Col, Val}, State) ->
    _ = notify_fun(Schema, MetricGroup, Col, Val, 0),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

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


%%====================================================================
%% INNTERNAL FUNCTION
%%====================================================================
%% @doc Create a schema with columns
%% @private
-spec(create_schema(list(atom())) ->
             ok | {error, any()}).
create_schema([]) ->
    {error, ?ERROR_COULD_NOT_CONNECT_SVM};
create_schema([Node|Rest]) ->
    case leo_rpc:call(Node, savanna_commons, create_schema,
                      [?QOS_METRIC_BUCKET_SCHEMA,
                       ?METRIC_BUCKET_COLS]) of
        ok ->
            ok;
        _ ->
            create_schema(Rest)
    end.


%% @doc Notify a message to savanna_agent
%%
-spec(notify_fun(binary(), binary(), binary(), any(), non_neg_integer())->
             ok).
notify_fun(_,_,_,_,?DEF_MAX_TIMES_OF_NOTICE) ->
    {error, "could not retrieve the schema"};
notify_fun(Schema, MetricGroup, Col, Val, Times) ->
    Ret = case svc_tbl_schema:get(Schema) of
              {ok, _} ->
                  ok;
              not_found ->
                  sync_tables_1(?env_qos_managers());
              {error, Cause} ->
                  {error, Cause}
          end,
    notify_fun_1(Ret, Schema, MetricGroup, Col, Val, Times).


%% @private
notify_fun_1(ok, Schema, MetricGroup, Col, Val, Times) ->
    case savanna_agent:notify(MetricGroup, Col, Val) of
        {error, undefined} ->
            savanna_agent:create_metrics(
              Schema, MetricGroup, ?METRIC_BUCKET_WINDOW),
            notify_fun(Schema, MetricGroup, Col, Val, Times + 1);
        Other ->
            Other
    end;
notify_fun_1(Other,_,_,_,_,_) ->
    Other.


%% @private
sync_tables_1([]) ->
    {error, ?ERROR_COULD_NOT_SYNC_TBL};
sync_tables_1([Node|Rest] = Managers) ->
    case leo_rpc:call(Node, svc_tbl_schema, get,
                      [?QOS_METRIC_BUCKET_SCHEMA]) of
        {ok, #?SV_SCHEMA{} = Schema} ->
            Ret = svc_tbl_schema:insert(Schema),
            sync_tables_2(Ret, Managers);
        _ ->
            sync_tables_1(Rest)
    end.

%% @private
sync_tables_2({error,_}, [_|Rest]) ->
    sync_tables_1(Rest);
sync_tables_2(_, []) ->
    {error, ?ERROR_COULD_NOT_SYNC_TBL};
sync_tables_2(ok = Ret, [Node|Rest]) ->
    case leo_rpc:call(Node, svc_tbl_column, find_by_schema_name,
                      [?QOS_METRIC_BUCKET_SCHEMA]) of
        {ok, Cols} ->
            case sync_tables_3(Cols) of
                ok ->
                    ok;
                {error, Cause} ->
                    {error, Cause}
            end;
        _ ->
            sync_tables_2(Ret, Rest)
    end.

%% @private
sync_tables_3([]) ->
    ok;
sync_tables_3([#?SV_COLUMN{} = Col|Rest]) ->
    case svc_tbl_column:insert(Col) of
        ok ->
            sync_tables_3(Rest);
        {error, Cause} ->
            {error, Cause}
    end.
