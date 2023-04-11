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
         get_role/2,
         list_roles/0
        ]).

-include("riak_cs.hrl").
-include("s3_api.hrl").
-include_lib("kernel/include/logger.hrl").


-spec create_role(proplist:proplist()) -> {ok, RoleId::string()} | {error, already_exists | term()}.
create_role(Specs) ->
    Role = exprec:fromlist_role_v1(Specs),
    Result = velvet:create_role("application/json",
                                binary_to_list(riak_cs_json:to_json(Role)), []),
    handle_response(Result).

-spec delete_role(string()) -> ok | {error, term()}.
delete_role(RoleId) ->
    Result = velvet:delete_role(RoleId, []),
    handle_response(Result).

-spec get_role(string(), pid()) -> {ok, ?S3_ROLE{}} | {error, term()}.
get_role(RoleId, RcPid) ->
    BinKey = list_to_binary(RoleId),
    case riak_cs_riak_client:get_role(RcPid, BinKey) of
        {ok, Obj} ->
            {ok, from_riakc_obj(Obj)};
        Error ->
            Error
    end.

from_riakc_obj(Obj) ->
    case riakc_obj:value_count(Obj) of
        1 ->
            binary_to_term(riakc_obj:get_value(Obj));
        0 ->
            error(no_value);
        N ->
            Values = [binary_to_term(Value) ||
                         Value <- riakc_obj:get_values(Obj),
                         Value /= <<>>  % tombstone
                     ],
            Role = hd(Values),
            logger:warning("Role object '~s' has ~b siblings", [Role?S3_ROLE.role_id, N]),
            Role
    end.

list_roles() ->
    logger:debug("STUB list_roles", []).


handle_response({ok, RoleId}) ->
    {ok, RoleId};
handle_response(ok) ->
    ok;
handle_response({error, {error_status, _, _, ErrorDoc}}) ->
    riak_cs_s3_response:error_response(ErrorDoc);
handle_response({error, _} = Error) ->
    Error.


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.
