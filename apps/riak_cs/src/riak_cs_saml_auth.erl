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

-module(riak_cs_saml_auth).

-behavior(riak_cs_auth).

-export([identify/2, authenticate/4]).

-include("riak_cs.hrl").
-include("saml_api.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-spec identify(#wm_reqdata{}, #rcs_context{}) -> failed | {string() | undefined, string()}.
identify(RD, #rcs_context{api=s3}) ->
    validate_federated_user(s3, RD);
identify(RD, #rcs_context{api=saml}) ->
    validate_federated_user(saml, wrq:get_req_header("x-auth-saml", RD)).

-spec authenticate(rcs_user(),
                   {string(), term()}|tuple(),
                   #wm_reqdata{}, #rcs_context{}) ->
                          ok | {error, invalid_authentication | missing_creds}.
authenticate(_User, {_, _}, _RD, _Ctx) ->
    {error, invalid_authentication}.

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_federated_user(_, undefined) ->
    failed;
validate_federated_user(Api, AuthToken) ->
    check_role(Api, AuthToken).

check_role(saml, RoleIdWithUserName) ->
    case string:tokens(RoleIdWithUserName, ":") of
        [RoleId, UserName] ->
            logger:debug("checking if role ~s exists for federated user ~s", [RoleId, UserName]),
            case riak_cs_roles:get_role(RoleId) of
                {ok, Role} ->
                    {role, Role};
                {error, not_found} ->
                    {error, no_such_role}
            end;
        _ ->
            {error, malformed_role}
    end.


%% ===================================================================
%% Eunit tests
%% ===================================================================

-ifdef(TEST).

-endif.
