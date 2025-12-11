%%======================================================================
%%
%% Leo Gateway - Logger Compatibility Header
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
%% This header provides compatibility macros for migrating from
%% leo_logger to Erlang's standard logger module (OTP 21+)
%%======================================================================

-ifndef(LEO_LOGGER_HRL).
-define(LEO_LOGGER_HRL, true).

%% Log level definitions
-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO, 1).
-define(LOG_LEVEL_WARN, 2).
-define(LOG_LEVEL_ERROR, 3).
-define(LOG_LEVEL_FATAL, 4).

%% Message log record for access logging
-record(message_log, {
    format :: string(),
    message :: list()
}).

%% Log appender types
-define(LOG_APPENDER_FILE, 'file').
-define(LOG_APPENDER_ESEARCH, 'esearch').

%% Log IDs
-define(LOG_ID_FILE_INFO, 'log_file_info').
-define(LOG_ID_FILE_ERROR, 'log_file_error').

%%----------------------------------------------------------------------
%% Logger Macros - Using Erlang Standard Logger
%%----------------------------------------------------------------------

%% Debug macro with function name and metadata
-define(debug(FuncName, Msg),
    logger:debug("~s: ~s", [FuncName, Msg],
                 #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                   line => ?LINE})).

-define(debug(FuncName, Format, Args),
    logger:debug("~s: " ++ Format, [FuncName | Args],
                 #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                   line => ?LINE})).

%% Info macro with function name and metadata
-define(info(FuncName, Msg),
    logger:info("~s: ~s", [FuncName, Msg],
                #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                  line => ?LINE})).

-define(info(FuncName, Format, Args),
    logger:info("~s: " ++ Format, [FuncName | Args],
                #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                  line => ?LINE})).

%% Warning macro with function name and metadata list
-define(warn(FuncName, MetadataList),
    logger:warning("~s: ~p", [FuncName, MetadataList],
                   #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                     line => ?LINE})).

-define(warn(FuncName, Format, Args),
    logger:warning("~s: " ++ Format, [FuncName | Args],
                   #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                     line => ?LINE})).

%% Error macro with function name and metadata list
-define(error(FuncName, MetadataList),
    logger:error("~s: ~p", [FuncName, MetadataList],
                 #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                   line => ?LINE})).

-define(error(FuncName, Format, Args),
    logger:error("~s: " ++ Format, [FuncName | Args],
                 #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                   line => ?LINE})).

%% Fatal macro (mapped to emergency level in OTP logger)
-define(fatal(FuncName, MetadataList),
    logger:emergency("~s: ~p", [FuncName, MetadataList],
                     #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                       line => ?LINE})).

-define(fatal(FuncName, Format, Args),
    logger:emergency("~s: " ++ Format, [FuncName | Args],
                     #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                       line => ?LINE})).

-endif.
