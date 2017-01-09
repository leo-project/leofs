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
-record(tcp_server_params, {
          prefix_of_name = "tcp_server_" :: string(),
          listen = [binary, {packet, line}, {active, false}, {reuseaddr, true}] :: [term()],
          port = 10010 :: pos_integer(),
          num_of_listeners = 3 :: pos_integer(),
          restart_times = 3 :: pos_integer(),
          time = 60 :: pos_integer(),
          shutdown = 2000  :: pos_integer(),
          accept_timeout = infinity :: infinity|non_neg_integer(),
          accept_error_sleep_time = 3000 :: pos_integer(),
          recv_length = 0 :: non_neg_integer(),
          recv_timeout = infinity :: infinity|non_neg_integer()
         }).
