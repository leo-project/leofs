%%====================================================================
%%
%% LeoStorage - Logger Macros using Erlang/OTP standard logger
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
%% -------------------------------------------------------------------
%% LeoFS Storage - Logger Macros (OTP 21+ logger)
%%
%%====================================================================
-ifndef(LEO_STORAGE_LOGGER_HRL).
-define(LEO_STORAGE_LOGGER_HRL, true).

%% Log level constants
-define(LOG_LEVEL_DEBUG, 0).
-define(LOG_LEVEL_INFO, 1).
-define(LOG_LEVEL_WARN, 2).
-define(LOG_LEVEL_ERROR, 3).
-define(LOG_LEVEL_FATAL, 4).

%% Logger macros using OTP standard logger
-define(debug(Fun, Msg),
        logger:debug(#{function => Fun, message => Msg},
                     #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                       file => ?FILE, line => ?LINE})).

-define(debug(Fun, Format, Args),
        logger:debug(#{function => Fun, format => Format, args => Args},
                     #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                       file => ?FILE, line => ?LINE})).

-define(info(Fun, Msg),
        logger:info(#{function => Fun, message => Msg},
                    #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                      file => ?FILE, line => ?LINE})).

-define(warn(Fun, Msg),
        logger:warning(#{function => Fun, message => Msg},
                       #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                         file => ?FILE, line => ?LINE})).

-define(error(Fun, Msg),
        logger:error(#{function => Fun, message => Msg},
                     #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                       file => ?FILE, line => ?LINE})).

-endif.
