%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2023 TI Tokyo    All Rights Reserved.
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

%% @doc riak_cs bucket utility functions, but I dare not use a module
%% name with '_utils'.

-module(riak_cs_roles).

-export([create_role/1,
         delete_role/1,
         get_role/1,
         list_roles/0
        ]).

-include("riak_cs.hrl").
-include("s3_api.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("riakc/include/riakc.hrl").
-include_lib("kernel/include/logger.hrl").

-spec create_role(maps:map()) -> ok | {error, already_exists}.
create_role(Specs) ->
    logger:debug("STUB create_role(~p)", [Specs]),
    Role = exprec:frommap_role_v1(Specs),
    Result = velvet:create_role("application/json",
                                binary_to_list(riak_cs_json:to_json(Role))),
    _ = riak_cs_stats:update_with_start(StatsKey, StartTime, Result),
    handle_create_role(Result, Role).

handle_create_user(ok, A) ->
    {ok, A};
handle_create_user({error, {error_status, _, _, ErrorDoc}}, _User) ->
    riak_cs_s3_response:error_response(ErrorDoc);
handle_create_user({error, _} = Error, _) ->
    Error.

delete_role(RoleId) ->
    logger:debug("STUB delete_role(~p)", [RoleId]).

get_role(RoleId) ->
    logger:debug("STUB get_role(~p)", [RoleId]).

list_roles() ->
    logger:debug("STUB list_roles", []).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.
