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
-module(leo_manager_transformer).

-include("leo_manager.hrl").
-include_lib("leo_redundant_manager/include/leo_redundant_manager.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([transform/0]).


%% @doc Migrate data
%%      - bucket  (s3-libs)
%%      - members (redundant-manager)
-spec(transform() ->
             ok).
transform() ->
    %% Update available commands
    ok = leo_manager_mnesia:update_available_commands(?env_available_commands()),

    %% data migration - members
    {ok, ReplicaNodes} = leo_misc:get_env(leo_redundant_manager, ?PROP_MNESIA_NODES),
    ok = leo_cluster_tbl_member:transform(ReplicaNodes),

    %% mdc-related
    CopyType = disc_copies,
    leo_mdcr_tbl_cluster_info:create_table(CopyType, ReplicaNodes),
    leo_mdcr_tbl_cluster_stat:create_table(CopyType, ReplicaNodes),
    leo_mdcr_tbl_cluster_mgr:create_table(CopyType, ReplicaNodes),
    leo_mdcr_tbl_cluster_member:create_table(CopyType, ReplicaNodes),

    %% data migration - redundant-manager related tables
    ok = leo_cluster_tbl_conf:transform(),
    ok = leo_mdcr_tbl_cluster_info:transform(),
    ok = leo_mdcr_tbl_cluster_stat:transform(),
    ok = leo_mdcr_tbl_cluster_member:transform(),
    ok = leo_ring_tbl_transformer:transform(),

    %% data migration - bucket
    case ?env_use_s3_api() of
        false -> void;
        true  ->
            {ok, #?SYSTEM_CONF{cluster_id = ClusterId}} = leo_cluster_tbl_conf:get(),
            ok = leo_s3_bucket:transform(),
            ok = leo_s3_bucket:transform(ClusterId),
            ok = leo_s3_user:transform()
    end,

    %% leo_statistics-related
    ok = leo_statistics_api:create_tables(CopyType, ReplicaNodes),
    ok = svc_tbl_column:transform(),
    ok = svc_tbl_schema:transform(),

    %% call plugin-mod for creating mnesia-table(s)
    case ?env_plugin_mod_mnesia() of
        undefined ->
            void;
        PluginModMnesia ->
            catch PluginModMnesia:call(CopyType, ReplicaNodes)
    end,
    ok.
