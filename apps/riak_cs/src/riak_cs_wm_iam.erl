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

%% @doc WM resource for IAM requests.

-module(riak_cs_wm_iam).

-export([init/1,
         service_available/2,
         malformed_request/2,
         forbidden/2,
         authorize/2,
         content_types_provided/2,
         content_types_accepted/2,
         generate_etag/2,
         last_modified/2,
         valid_entity_length/2,
         multiple_choices/2,
         accept_wwwform/2,
         allowed_methods/2,
         post_is_create/2,
         create_path/2,
         finish_request/2
        ]).

-ignore_xref([init/1,
              service_available/2,
              malformed_request/2,
              forbidden/2,
              authorize/2,
              content_types_provided/2,
              content_types_accepted/2,
              generate_etag/2,
              last_modified/2,
              multiple_choices/2,
              authorize/2,
              accept_wwwform/2,
              allowed_methods/2,
              valid_entity_length/2,
              post_is_create/2,
              create_path/2,
              finish_request/2
             ]).

-include("riak_cs_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-include_lib("kernel/include/logger.hrl").

%% -------------------------------------------------------------------
%% Webmachine callbacks
%% -------------------------------------------------------------------

-spec init([proplists:proplist()]) -> {ok, #rcs_iam_context{}}.
init(Config) ->
    %% Check if authentication is disabled and set that in the context.
    AuthBypass = proplists:get_value(auth_bypass, Config),
    AuthModule = proplists:get_value(auth_module, Config),
    Api = riak_cs_config:api(),
    RespModule = riak_cs_config:response_module(Api),
    StatsPrefix = no_stats,
    Ctx = #rcs_iam_context{auth_bypass = AuthBypass,
                           auth_module = AuthModule,
                           response_module = RespModule,
                           stats_prefix = StatsPrefix,
                           api = Api},
    {ok, Ctx}.


-spec service_available(#wm_reqdata{}, #rcs_iam_context{}) -> {boolean(), #wm_reqdata{}, #rcs_iam_context{}}.
service_available(RD, Ctx = #rcs_iam_context{rc_pool = undefined}) ->
    ?LOG_DEBUG("initingg", []),
    service_available(RD, Ctx#rcs_iam_context{rc_pool = request_pool});
service_available(RD, Ctx = #rcs_iam_context{rc_pool = Pool}) ->
    case riak_cs_riak_client:checkout(Pool) of
        {ok, RcPid} ->
            {true, RD, Ctx#rcs_iam_context{riak_client = RcPid}};
        {error, _Reason} ->
            {false, RD, Ctx}
    end.

-spec malformed_request(#wm_reqdata{}, #rcs_iam_context{}) -> {boolean(), #wm_reqdata{}, #rcs_iam_context{}}.
malformed_request(RD, Ctx) ->
    ?LOG_DEBUG("initingg", []),
    {false, RD, Ctx}.


-spec valid_entity_length(#wm_reqdata{}, #rcs_iam_context{}) -> {boolean(), #wm_reqdata{}, #rcs_iam_context{}}.
valid_entity_length(RD, Ctx) ->
    ?LOG_DEBUG("initingg", []),
    {true, RD, Ctx}.


-spec forbidden(#wm_reqdata{}, #rcs_iam_context{}) ->
          {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_iam_context{}}.
forbidden(RD, Ctx=#rcs_iam_context{auth_module = AuthMod,
                                   riak_client = RcPid}) ->
    ?LOG_DEBUG("initingg", []),
    AuthResult =
        case AuthMod:identify(RD, Ctx) of
            failed ->
                %% Identification failed, deny access
                {error, no_such_key};
            {failed, Reason} ->
                {error, Reason};
            {UserKey, AuthData} ->
                case riak_cs_user:get_user(UserKey, RcPid) of
                    {ok, {User, Obj}} = _LookupResult ->
                        authenticate(User, Obj, RD, Ctx, AuthData);
                    Error ->
                        Error
                end;
            Role ->
                riak_cs_wm_utils:eval_role_for_action(RD, Role)
        end,
    post_authentication(AuthResult, RD, Ctx, fun authorize/2).

post_authentication({ok, User, _UserObj}, RD, Ctx, Authorize) ->
    %% given keyid and signature matched, proceed
    Authorize(RD, Ctx#rcs_iam_context{user = User});
post_authentication({error, no_user_key}, RD, Ctx, Authorize) ->
    %% no keyid was given, proceed anonymously
    ?LOG_DEBUG("No user key"),
    Authorize(RD, Ctx);
post_authentication({error, bad_auth}, RD, #rcs_iam_context{response_module = ResponseMod} = Ctx, _) ->
    %% given keyid was found, but signature didn't match
    ?LOG_DEBUG("bad_auth"),
    ResponseMod:api_error(access_denied, RD, Ctx);
post_authentication({error, reqtime_tooskewed} = Error, RD,
                    #rcs_iam_context{response_module = ResponseMod} = Ctx, _) ->
    ?LOG_DEBUG("reqtime_tooskewed"),
    ResponseMod:api_error(Error, RD, Ctx);
post_authentication({error, {auth_not_supported, AuthType}}, RD,
                    #rcs_iam_context{response_module = ResponseMod} = Ctx, _) ->
    ?LOG_DEBUG("auth_not_supported: ~s", [AuthType]),
    ResponseMod:api_error({auth_not_supported, AuthType}, RD, Ctx);
post_authentication({error, notfound}, RD, #rcs_iam_context{response_module = ResponseMod} = Ctx, _) ->
    ?LOG_DEBUG("User does not exist"),
    ResponseMod:api_error(invalid_access_key_id, RD, Ctx);
post_authentication({error, Reason}, RD,
                    #rcs_iam_context{response_module = ResponseMod} = Ctx, _) ->
    %% Lookup failed, basically due to disconnected stuff
    ?LOG_DEBUG("Authentication error: ~p", [Reason]),
    ResponseMod:api_error(Reason, RD, Ctx).

authenticate(User, UserObj, RD, Ctx = #rcs_iam_context{auth_module = AuthMod}, AuthData)
  when User?RCS_USER.status =:= enabled ->
    case AuthMod:authenticate(User, AuthData, RD, Ctx) of
        ok ->
            {ok, User, UserObj};
        {error, reqtime_tooskewed} ->
            {error, reqtime_tooskewed};
        {error, _Reason} ->
            {error, bad_auth}
    end;
authenticate(User, _UserObj, _RD, _Ctx, _AuthData)
  when User?RCS_USER.status =/= enabled ->
    %% {ok, _} -> %% disabled account, we are going to 403
    {error, bad_auth}.

-spec allowed_methods(#wm_reqdata{}, #rcs_iam_context{}) -> {[atom()], #wm_reqdata{}, #rcs_iam_context{}}.
allowed_methods(RD, Ctx) ->
    {['POST'], RD, Ctx}.


-spec content_types_accepted(#wm_reqdata{}, #rcs_iam_context{}) ->
          {[{string(), module()}], #wm_reqdata{}, #rcs_iam_context{}}.
content_types_accepted(RD, Ctx) ->
    {[{?WWWFORM_TYPE, accept_wwwform}], RD, Ctx}.


-spec content_types_provided(#wm_reqdata{}, #rcs_iam_context{}) ->
          {[{string(), module()}], #wm_reqdata{}, #rcs_iam_context{}}.
content_types_provided(RD, Ctx) ->
    {[{?XML_TYPE, produce_xml}], RD, Ctx}.


-spec authorize(#wm_reqdata{}, #rcs_iam_context{}) ->
          {boolean() | {halt, term()}, #wm_reqdata{}, #rcs_iam_context{}}.
authorize(RD, Ctx) ->
    riak_cs_wm_utils:role_access_authorize_helper(RD, Ctx).


-spec generate_etag(#wm_reqdata{}, #rcs_iam_context{}) -> {undefined|string(), #wm_reqdata{}, #rcs_iam_context{}}.
generate_etag(RD, Ctx) ->
    {undefined, RD, Ctx}.


-spec last_modified(#wm_reqdata{}, #rcs_iam_context{}) -> {undefined|string(), #wm_reqdata{}, #rcs_iam_context{}}.
last_modified(RD, Ctx) ->
    {undefined, RD, Ctx}.

-spec post_is_create(#wm_reqdata{}, #rcs_iam_context{}) ->
          {true, #wm_reqdata{}, #rcs_iam_context{}}.
post_is_create(RD, Ctx) ->
    {true, RD, Ctx}.


-spec create_path(#wm_reqdata{}, #rcs_iam_context{}) ->
          {string(), #wm_reqdata{}, #rcs_iam_context{}}.
create_path(RD, Ctx) ->
    {wrq:disp_path(RD), RD, Ctx}.


-spec multiple_choices(#wm_reqdata{}, #rcs_iam_context{}) ->
          {boolean(), #wm_reqdata{}, #rcs_iam_context{}}.
multiple_choices(RD, Ctx) ->
    {false, RD, Ctx}.


-spec accept_wwwform(#wm_reqdata{}, #rcs_iam_context{}) ->
          {boolean() | {halt, term()}, term(), term()}.
accept_wwwform(RD, Ctx) ->
    Form = mochiweb_util:parse_qs(wrq:req_body(RD)),
    Action = proplists:get_value("Action", Form),
    do_action(Action, Form, RD, Ctx).

-spec finish_request(#wm_reqdata{}, #rcs_iam_context{}) ->
          {boolean() | {halt, term()}, term(), term()}.
finish_request(RD, Ctx=#rcs_iam_context{riak_client = undefined}) ->
    {true, RD, Ctx};
finish_request(RD, Ctx=#rcs_iam_context{riak_client=RcPid}) ->
    riak_cs_riak_client:checkin(RcPid),
    {true, RD, Ctx#rcs_iam_context{riak_client = undefined}}.


%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

do_action("CreateRole", Form, RD, Ctx) ->
    Specs = lists:foldl(fun role_fields_filter/2, [], Form),
    make_create_role_response(
      riak_cs_roles:create_role(Specs),
      Specs, RD, Ctx);

do_action("GetRole", Form, RD, Ctx = #rcs_iam_context{riak_client = RcPid}) ->
    RoleName = proplists:get_value("RoleName", Form),
    make_get_role_response(
      riak_cs_roles:get_role(RoleName, RcPid),
      RD, Ctx);

do_action("ListRoles", Form, RD, Ctx = #rcs_iam_context{riak_client = RcPid}) ->
    PathPrefix = proplists:get_value("PathPrefix", Form),
    make_list_roles_response(
      riak_cs_api:list_roles(RcPid, ?LRREQ{path_prefix = PathPrefix}),
      RD, Ctx);

do_action(Unsupported, _Form, RD, Ctx = #rcs_iam_context{response_module = ResponseMod}) ->
    logger:warning("IAM action ~s not supported yet; ignoring request", [Unsupported]),
    ResponseMod:api_error(unsupported_iam_action, RD, Ctx).


role_fields_filter({ItemKey, ItemValue}, Acc) ->
    case ItemKey of
        "AssumeRolePolicyDocument" ->
            [{assume_role_policy_document, base64:encode(ItemValue)} | Acc];
        "Description" ->
            [{description, ItemValue} | Acc];
        "MaxSessionDuration" ->
            [{max_session_duration, ItemValue} | Acc];
        "Path" ->
            [{path, ItemValue} | Acc];
        "PermissionsBoundary" ->
            [{permissions_boundary, ItemValue} | Acc];
        "RoleName" ->
            [{role_name, ItemValue} | Acc];
        "Tags" ->
            [{tags, ItemValue} | Acc];
        _ ->
            Acc
    end.


make_create_role_response({ok, RoleId}, Specs, RD, Ctx) ->
    Role_ = ?IAM_ROLE{assume_role_policy_document = A} = exprec:fromlist_role_v1(Specs),
    Role = Role_?IAM_ROLE{assume_role_policy_document = binary_to_list(base64:decode(A)),
                          role_id = RoleId},
    RequestId = make_request_id(),
    logger:info("Created role \"~s\" on request_id ~s", [Role?IAM_ROLE.role_id, RequestId]),
    Doc = riak_cs_xml:to_xml(
            #create_role_response{role = Role,
                                  request_id = RequestId}),
    {true, make_final_rd(Doc, RD), Ctx};
make_create_role_response({error, Reason}, _, RD, Ctx) ->
    riak_cs_s3_response:api_error(Reason, RD, Ctx).


make_get_role_response({ok, Role}, RD, Ctx) ->
    RequestId = make_request_id(),
    Doc = riak_cs_xml:to_xml(
            #get_role_response{role = Role,
                               request_id = RequestId}),
    {true, make_final_rd(Doc, RD), Ctx};
make_get_role_response({error, not_found}, RD, Ctx = #rcs_iam_context{response_module = ResponseMod}) ->
    ResponseMod:api_error(no_such_role, RD, Ctx).


make_list_roles_response({ok, Roles}, RD, Ctx) ->
    RequestId = make_request_id(),
    Doc = riak_cs_xml:to_xml(
            #list_roles_response{roles = Roles,
                                 request_id = RequestId}),
    {true, make_final_rd(Doc, RD), Ctx};
make_list_roles_response(Error, RD, Ctx = #rcs_iam_context{response_module = ResponseMod}) ->
    ResponseMod:api_error(Error, RD, Ctx).



make_final_rd(Body, RD) ->
    wrq:set_resp_body(
      Body, wrq:set_resp_header(
              "ETag", etag(Body),
              wrq:set_resp_header(
                "Content-Type", ?XML_TYPE, RD))).

make_request_id() ->
    uuid:uuid_to_string(uuid:get_v4()).

etag(Body) ->
        riak_cs_utils:etag_from_binary(riak_cs_utils:md5(Body)).

