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

-spec identify(#wm_reqdata{}, #rcs_context{}) -> failed | {string() | undefined , string()}.
identify(RD, #rcs_context{api=s3}) ->
    validate_saml(s3, RD);
identify(RD, #rcs_context{api=saml}) ->
    validate_saml(saml, wrq:get_req_header("x-auth-saml", RD)).

-spec authenticate(rcs_user(),
                   {string(), term()}|tuple(),
                   #wm_reqdata{}, #rcs_context{}) ->
                          ok | {error, invalid_authentication | missing_creds}.
authenticate(_User, {_, _TokenItems}, _RD, _Ctx) ->
    {error, invalid_authentication}.

%% ===================================================================
%% Internal functions
%% ===================================================================

validate_saml(_, undefined) ->
    failed;
validate_saml(Api, AuthToken) ->
    initiate_and_handle_idp_response(Api, AuthToken).

initiate_and_handle_idp_response(saml, AuthToken) ->
    case extract_user_creds(AuthToken) of
        {ok, {User, Passwd}} ->
            logger:debug("initiating a SAML flow with ~p", [riak_cs_config:saml_idp_host()]),
            %% log the caller in, start an ephemeral session
            ok = generate_saml_request(User, Passwd),
            validate_idp_response();
        _ ->
            {error, missing_creds}
    end.

extract_user_creds(_AuthHeader) ->
    {error, missing_creds}.

generate_saml_request(_User, _Passwd) ->
    ok.

validate_idp_response() ->
    {error, invalid_authentication}.


%% ===================================================================
%% Eunit tests
%% ===================================================================

-ifdef(TEST).

-endif.
