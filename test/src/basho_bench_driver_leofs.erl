%% -------------------------------------------------------------------
%%
%% basho_bench: Benchmarking Suite 
%%
%% Copyright (c) 2012 Rakuten, Inc.
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
-module(basho_bench_driver_leofs).

-export([new/1,
         run/4]).

-include("basho_bench.hrl").
-include("leo_s3_auth.hrl").

-record(url, {abspath, host, port, username, password, path, protocol, host_type}).

-record(state, { base_urls,          % Tuple of #url -- one for each IP
                 base_urls_index,    % #url to use for next request
                 path_params }).     % Params to append on the path

-define(S3_ACC_KEY,      "05236").
-define(S3_SEC_KEY,      "802562235").
-define(S3_CONTENT_TYPE, "application/octet-strea").

%% ====================================================================
%% API
%% ====================================================================
new(_Id) ->
    %% Make sure ibrowse is available
    case code:which(ibrowse) of
        non_existing ->
            ?FAIL_MSG("~s requires ibrowse to be installed.\n", [?MODULE]);
        _ ->
            ok
    end,

    application:start(ibrowse),

    %% The IPs, port and path we'll be testing
    Ips  = basho_bench_config:get(http_raw_ips,      ["localhost"]),
    Port = basho_bench_config:get(http_raw_port,     8080),
    Path = basho_bench_config:get(http_raw_path,     "/bbb/_test"),
    Params = basho_bench_config:get(http_raw_params, ""),
    Disconnect = basho_bench_config:get(http_raw_disconnect_frequency, infinity),

    case Disconnect of
        infinity -> ok;
        Seconds when is_integer(Seconds) -> ok;
        {ops, Ops} when is_integer(Ops) -> ok;
        _ -> ?FAIL_MSG("Invalid configuration for http_raw_disconnect_frequency: ~p~n", [Disconnect])
    end,

    %% Uses pdict to avoid threading state record through lots of functions
    erlang:put(disconnect_freq, Disconnect),

    %% If there are multiple URLs, convert the list to a tuple so we can efficiently
    %% round-robin through them.
    case length(Ips) of
        1 ->
            [Ip] = Ips,
            BaseUrls = #url { host = Ip, port = Port, path = Path },
            BaseUrlsIndex = 1;
        _ ->
            BaseUrls = list_to_tuple([ #url { host = Ip, port = Port, path = Path }
                                       || Ip <- Ips]),
            BaseUrlsIndex = random:uniform(tuple_size(BaseUrls))
    end,

    {ok, #state { base_urls = BaseUrls,
                  base_urls_index = BaseUrlsIndex,
                  path_params = Params }}.


run(get, KeyGen, _ValueGen, State) ->
    {NextUrl, S2} = next_url(State),
    case do_get(url(NextUrl, KeyGen, State#state.path_params)) of
        {not_found, _Url} ->
            {ok, S2};
        {ok, _Url, _Headers} ->
            {ok, S2};
        {error, Reason} ->
            io:format("~p~n",[Reason]),
            {error, Reason, S2}
    end;


run(put, KeyGen, ValueGen, State) ->
    {NextUrl, S2} = next_url(State),
    Url = url(NextUrl, KeyGen, State#state.path_params),

    case do_put(Url, [], ValueGen) of
        ok ->
            {ok, S2};
        {error, Reason} ->
            {error, Reason, S2}
    end;


run(delete, KeyGen, _ValueGen, State) ->
    {NextUrl, S2} = next_url(State),
    %% Url = url(NextUrl, KeyGen, State#state.path_params),

    case do_delete(url(NextUrl, KeyGen, State#state.path_params)) of
        {not_found, _Url} ->
            {ok, S2};
        {ok, _Url, _Headers} ->
            {ok, S2};
        {error, Reason} ->
            {error, Reason, S2}
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================

next_url(State) when is_record(State#state.base_urls, url) ->
    {State#state.base_urls, State};

next_url(State) when State#state.base_urls_index > tuple_size(State#state.base_urls) ->
    { element(1, State#state.base_urls),
      State#state { base_urls_index = 1 } };

next_url(State) ->
    { element(State#state.base_urls_index, State#state.base_urls),
      State#state { base_urls_index = State#state.base_urls_index + 1 }}.

%% url(BaseUrl, Params) ->
%%     BaseUrl#url { path = lists:concat([BaseUrl#url.path, Params]) }.
url(BaseUrl, KeyGen, Params) ->
    BaseUrl#url { path = lists:concat([BaseUrl#url.path, '/', KeyGen(), Params]) }.


do_get(Url) ->
    case send_request(Url, [], get, [], [{response_format, binary}]) of
        {ok, "404", _Headers, _Body} ->
            {not_found, Url};
        {ok, "200", Headers, _Body} ->
            {ok, Url, Headers};
        {ok, Code, _Headers, _Body} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

gen_sig(HTTPMethod, Url) ->
    [Bucket|_] = string:tokens(Url#url.path, "/"),
    SignParams = #sign_params{http_verb    = HTTPMethod,
                              content_md5  = [],
                              content_type = ?S3_CONTENT_TYPE,
                              date         = [],
                              bucket       = Bucket,
                              uri          = Url#url.path,
                              query_str    = [],
                              amz_headers  = []
                              },
    Sig = leo_s3_auth:get_signature(?S3_SEC_KEY, SignParams),
    io_lib:format("AWS ~s:~s", [?S3_ACC_KEY, Sig]).
    
do_put(Url, Headers, ValueGen) ->
    case send_request(Url, Headers ++ [{'Content-Type', ?S3_CONTENT_TYPE}, {'Authorization', gen_sig("PUT", Url)}],
                      put, ValueGen(), [{response_format, binary}]) of
        {ok, "200", _Header, _Body} ->
            ok;
        {ok, "204", _Header, _Body} ->
            ok;
        {ok, Code, _Header, _Body} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

%% do_post(Url, Headers, ValueGen) ->
%%     case send_request(Url, Headers ++ [{'Content-Type', 'application/octet-stream'}],
%%                       post, ValueGen(), [{response_format, binary}]) of
%%         {ok, "200", _Header, _Body} ->
%%             ok;
%%         {ok, "201", _Header, _Body} ->
%%             ok;
%%         {ok, "204", _Header, _Body} ->
%%             ok;
%%         {ok, Code, _Header, _Body} ->
%%             {error, {http_error, Code}};
%%         {error, Reason} ->
%%            {error, Reason}
%%     end.

do_delete(Url) ->
    case send_request(Url, [{'Authorization', gen_sig("DELETE", Url)}], delete, [], [{response_format, binary}]) of
        {ok, "404", _Headers, _Body} ->
            {not_found, Url};
        {ok, "200", Headers, _Body} ->
            {ok, Url, Headers};
        {ok, Code, _Headers, _Body} ->
            {error, {http_error, Code}};
        {error, Reason} ->
            {error, Reason}
    end.

connect(Url) ->
    case erlang:get({ibrowse_pid, Url#url.host}) of
        undefined ->
            {ok, Pid} = ibrowse_http_client:start({Url#url.host, Url#url.port}),
            erlang:put({ibrowse_pid, Url#url.host}, Pid),
            Pid;
        Pid ->
            case is_process_alive(Pid) of
                true ->
                    Pid;
                false ->
                    erlang:erase({ibrowse_pid, Url#url.host}),
                    connect(Url)
            end
    end.


disconnect(Url) ->
    case erlang:get({ibrowse_pid, Url#url.host}) of
        undefined ->
            ok;
        OldPid ->
            catch(ibrowse_http_client:stop(OldPid))
    end,
    erlang:erase({ibrowse_pid, Url#url.host}),
    ok.

maybe_disconnect(Url) ->
    case erlang:get(disconnect_freq) of
        infinity -> ok;
        {ops, Count} -> should_disconnect_ops(Count,Url) andalso disconnect(Url);
        Seconds -> should_disconnect_secs(Seconds,Url) andalso disconnect(Url)
    end.

should_disconnect_ops(Count, Url) ->
    Key = {ops_since_disconnect, Url#url.host},
    case erlang:get(Key) of
        undefined ->
            erlang:put(Key, 1),
            false;
        Count ->
            erlang:put(Key, 0),
            true;
        Incr ->
            erlang:put(Key, Incr + 1),
            false
    end.

should_disconnect_secs(Seconds, Url) ->
    Key = {last_disconnect, Url#url.host},
    case erlang:get(Key) of
        undefined ->
            erlang:put(Key, erlang:now()),
            false;
        Time when is_tuple(Time) andalso size(Time) == 3 ->
            Diff = timer:now_diff(erlang:now(), Time),
            if
                Diff >= Seconds * 1000000 ->
                    erlang:put(Key, erlang:now()),
                    true;
                true -> false
            end
    end.

clear_disconnect_freq(Url) ->
    case erlang:get(disconnect_freq) of
        infinity -> ok;
        {ops, _Count} -> erlang:put({ops_since_disconnect, Url#url.host}, 0);
        _Seconds -> erlang:put({last_disconnect, Url#url.host}, erlang:now())
    end.

send_request(Url, Headers, Method, Body, Options) ->
    send_request(Url, Headers, Method, Body, Options, 3).

send_request(_Url, _Headers, _Method, _Body, _Options, 0) ->
    {error, max_retries};
send_request(Url, Headers, Method, Body, Options, Count) ->
    Pid = connect(Url),
    case catch(ibrowse_http_client:send_req(
           Pid, Url, Headers, Method, Body, Options,
           basho_bench_config:get(http_raw_request_timeout, 5000))) of

        {ok, Status, RespHeaders, RespBody} ->
            maybe_disconnect(Url),
            {ok, Status, RespHeaders, RespBody};

        Error ->
            clear_disconnect_freq(Url),
            disconnect(Url),
            case should_retry(Error) of
                true ->
                    send_request(Url, Headers, Method, Body, Options, Count-1);

                false ->
                    normalize_error(Method, Error)
            end
    end.


should_retry({error, send_failed})       -> true;
should_retry({error, connection_closed}) -> true;
should_retry({'EXIT', {normal, _}})      -> true;
should_retry({'EXIT', {noproc, _}})      -> true;
should_retry(_)                          -> false.

normalize_error(Method, {'EXIT', {timeout, _}})  -> {error, {Method, timeout}};
normalize_error(Method, {'EXIT', Reason})        -> {error, {Method, 'EXIT', Reason}};
normalize_error(Method, {error, Reason})         -> {error, {Method, Reason}}.


