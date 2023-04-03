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

-module(riak_cs_roles).

-export([create_role/1,
         delete_role/1,
         get_role/1,
         list_roles/0
        ]).

-include("riak_cs.hrl").
-include("s3_api.hrl").
-include_lib("kernel/include/logger.hrl").

-define(ROLE_ID_LENGTH, 21).  %% length("AROAJQABLZS4A3QDU576Q").
-define(ROLE_ID_CHARSET, "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789").

-spec create_role(proplist:proplist()) -> {ok, RoleId::string()} | {error, already_exists | term()}.
create_role(Specs) ->
    Role0 = exprec:fromlist_role_v1(Specs),
    RoleId = make_role_id(),
    ?LOG_INFO("Creating role with id ~s", [RoleId]),
    Role1 = Role0?S3_ROLE{role_id = RoleId},
    Result = velvet:create_role("application/json",
                                binary_to_list(riak_cs_json:to_json(Role))),
    _ = riak_cs_stats:update_with_start(StatsKey, StartTime, Result),
    handle_create_role(Result, RoleId).

make_role_id() ->
    fill(?ROLE_ID_LENGTH - 4, "AROA").
fill(0, Q) ->
    Q;
fill(N, Q) ->
    fill(N-1, Q ++ [lists:nth(rand:uniform(length(?ROLE_ID_CHARSET)))]).

handle_create_role(ok, A) ->
    {ok, A};
handle_create_role({error, {error_status, _, _, ErrorDoc}}, _User) ->
    riak_cs_s3_response:error_response(ErrorDoc);
handle_create_role({error, _} = Error, _) ->
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
