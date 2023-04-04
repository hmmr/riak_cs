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

%% @doc WM resource for listing roles

-module(riak_cs_wm_role).

-export([init/1,
         service_available/2,
         forbidden/2,
         content_types_provided/2,
         content_types_accepted/2,
         accept_json/2,
         accept_xml/2,
         allowed_methods/2,
         post_is_create/2,
         produce_json/2,
         produce_xml/2,
         finish_request/2
        ]).

-ignore_xref([init/1,
              service_available/2,
              forbidden/2,
              content_types_provided/2,
              content_types_accepted/2,
              accept_json/2,
              accept_xml/2,
              allowed_methods/2,
              post_is_create/2,
              produce_json/2,
              produce_xml/2,
              finish_request/2
             ]).

-include("riak_cs_web.hrl").
-include_lib("webmachine/include/webmachine.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%% -------------------------------------------------------------------
%% Webmachine callbacks
%% -------------------------------------------------------------------

-spec init(proplists:proplist()) -> {ok, #rcs_context{}}.
init(Config) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"init">>),
    %% Check if authentication is disabled and
    %% set that in the context.
    AuthBypass = not proplists:get_value(admin_auth_enabled, Config),
    Api = riak_cs_config:api(),
    RespModule = riak_cs_config:response_module(Api),
    {ok, #rcs_context{auth_bypass=AuthBypass,
                      api=Api,
                      response_module=RespModule}}.

-spec service_available(#wm_reqdata{}, #rcs_context{}) -> {true, #wm_reqdata{}, #rcs_context{}}.
service_available(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"service_available">>),
    riak_cs_wm_utils:service_available(RD, Ctx).

-spec allowed_methods(#wm_reqdata{}, #rcs_context{}) -> {[atom()], #wm_reqdata{}, #rcs_context{}}.
allowed_methods(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"allowed_methods">>),
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

-spec forbidden(#wm_reqdata{}, #rcs_context{}) ->
          {boolean() | {halt, non_neg_integer()}}, #wm_reqdata{}, #rcs_context{}}.
forbidden(RD, Ctx=#rcs_context{auth_bypass=AuthBypass}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"forbidden">>),
    Method = wrq:method(RD),
    AnonOk = (Method =:= 'POST') orelse AuthBypass,
    Next = fun(NewRD, NewCtx=#rcs_context{user=User}) ->
                   forbidden(wrq:method(RD),
                             NewRD,
                             NewCtx,
                             User,
                             user_key(RD),
                             AnonOk)
           end,
    UserAuthResponse = riak_cs_wm_utils:find_and_auth_user(RD, Ctx, Next),
    handle_user_auth_response(UserAuthResponse).

handle_user_auth_response({false, _RD, Ctx} = Ret) ->
    riak_cs_dtrace:dt_wm_return(?MODULE, <<"forbidden">>,
                                [], [riak_cs_wm_utils:extract_name(Ctx#rcs_context.user), <<"false">>]),
    Ret;
handle_user_auth_response({{halt, Code}, _RD, Ctx} = Ret) ->
    riak_cs_dtrace:dt_wm_return(?MODULE, <<"forbidden">>,
                                [Code], [riak_cs_wm_utils:extract_name(Ctx#rcs_context.user), <<"true">>]),
    Ret;
handle_user_auth_response({_Reason, _RD, Ctx} = Ret) ->
    riak_cs_dtrace:dt_wm_return(?MODULE, <<"forbidden">>,
                                [-1], [riak_cs_wm_utils:extract_name(Ctx#rcs_context.user), <<"true">>]),
    Ret.

-spec content_types_accepted(#wm_reqdata{}, #rcs_context{}) ->
    {[{string(), atom()}], #wm_reqdata{}, #rcs_context{}}.
content_types_accepted(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"content_types_accepted">>),
    {[{?XML_TYPE, accept_xml}, {?JSON_TYPE, accept_json}], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"content_types_provided">>),
    {[{?XML_TYPE, produce_xml}, {?JSON_TYPE, produce_json}], RD, Ctx}.

post_is_create(RD, Ctx) -> {true, RD, Ctx}.

-spec accept_json(#wm_reqdata{}, #rcs_context{}) ->
    {boolean() | {halt, term()}, term(), term()}.
accept_json(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"accept_json">>),
    Specs = maps:from_list(
              riak_cs_json:from_json(wrq:req_body(RD))),
    role_response(
      riak_cs_roles:create_role(Specs),
      ?JSON_TYPE, RD, Ctx).

-spec accept_xml(#wm_reqdata{}, #rcs_context{}) ->
    {boolean() | {halt, term()}, #wm_reqdata{}, #rcs_context{}}.
accept_xml(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"accept_xml">>),
    case riak_cs_xml:scan(binary_to_list(wrq:req_body(RD))) of
        {error, malformed_xml} ->
            riak_cs_s3_response:api_error(invalid_role_parameters, RD, Ctx);
        {ok, ParsedData} ->
            Specs =
                lists:foldl(fun role_xml_filter/2, [], ParsedData#xmlElement.content),
            role_response(
              riak_cs_roles:create_role(Specs),
              ?XML_TYPE, RD, Ctx)
    end.

produce_json(RD, #rcs_context{}=Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"produce_json">>),
    Body = riak_cs_json:to_json(
             riak_cs_role:get_role(RoleId)),
    Etag = etag(Body),
    RD2 = wrq:set_resp_header("ETag", Etag, RD),
    {Body, RD2, Ctx}.

produce_xml(RD, #rcs_context{}=Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"produce_xml">>),
    Body = riak_cs_xml:to_xml(
             riak_cs_role:get_role(Roleid)),
    Etag = etag(Body),
    RD2 = wrq:set_resp_header("ETag", Etag, RD),
    {Body, RD2, Ctx}.

finish_request(RD, Ctx=#rcs_context{riak_client=undefined}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"finish_request">>, [0], []),
    {true, RD, Ctx};
finish_request(RD, Ctx=#rcs_context{riak_client=RcPid}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"finish_request">>, [1], []),
    riak_cs_riak_client:checkin(RcPid),
    riak_cs_dtrace:dt_wm_return(?MODULE, <<"finish_request">>, [1], []),
    {true, RD, Ctx#rcs_context{riak_client=undefined}}.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

admin_check(true, RD, Ctx) ->
    {false, RD, Ctx#rcs_context{user=undefined}};
admin_check(false, RD, Ctx) ->
    riak_cs_wm_utils:deny_access(RD, Ctx).

%% @doc Calculate the etag of a response body
etag(Body) ->
        riak_cs_utils:etag_from_binary(riak_cs_utils:md5(Body)).

forbidden(_Method, RD, Ctx, undefined, _UserPathKey, false) ->
    %% anonymous access disallowed
    riak_cs_wm_utils:deny_access(RD, Ctx);
forbidden(_, _RD, _Ctx, undefined, [], true) ->
    {false, _RD, _Ctx};
forbidden(_, RD, Ctx, undefined, UserPathKey, true) ->
    get_user({false, RD, Ctx}, UserPathKey);
forbidden('POST', RD, Ctx, User, [], _) ->
    %% Admin is creating a new user
    admin_check(riak_cs_user:is_admin(User), RD, Ctx);
forbidden(_Method, RD, Ctx, User, UserPathKey, _) when
      UserPathKey =:= User?RCS_USER.key_id;
      UserPathKey =:= [] ->
    %% User is accessing own account
    AccessRD = riak_cs_access_log_handler:set_user(User, RD),
    {false, AccessRD, Ctx};
forbidden(_Method, RD, Ctx, User, UserPathKey, _) ->
    AdminCheckResult = admin_check(riak_cs_user:is_admin(User), RD, Ctx),
    get_user(AdminCheckResult, UserPathKey).

get_user({false, RD, Ctx}, UserPathKey) ->
    handle_get_user_result(
      riak_cs_user:get_user(UserPathKey, Ctx#rcs_context.riak_client),
      RD, Ctx);
get_user(AdminCheckResult, _) ->
    AdminCheckResult.

handle_get_user_result({ok, {User, UserObj}}, RD, Ctx) ->
    {false, RD, Ctx#rcs_context{user=User, user_object=UserObj}};
handle_get_user_result({error, Reason}, RD, Ctx) ->
    logger:warning("Failed to fetch user record. KeyId: ~p"
                   " Reason: ~p", [user_key(RD), Reason]),
    riak_cs_s3_response:api_error(invalid_access_key_id, RD, Ctx).


set_resp_data(ContentType, RD, #rcs_context{user=User}) ->
    UserDoc = format_user_record(User, ContentType),
    wrq:set_resp_body(UserDoc, RD).


user_json_filter({ItemKey, ItemValue}, Acc) ->
    case ItemKey of
        <<"email">> ->
            [{email, binary_to_list(ItemValue)} | Acc];
        <<"name">> ->
            [{name, binary_to_list(ItemValue)} | Acc];
        <<"status">> ->
            case ItemValue of
                <<"enabled">> ->
                    [{status, enabled} | Acc];
                <<"disabled">> ->
                    [{status, disabled} | Acc];
                _ ->
                    Acc
            end;
        <<"new_key_secret">> ->
            [{new_key_secret, ItemValue} | Acc];
        _ ->
            Acc
    end.

user_key(RD) ->
    case wrq:path_tokens(RD) of
        [KeyId|_] -> mochiweb_util:unquote(KeyId);
        _         -> []
    end.

-spec user_xml_filter(#xmlText{} | #xmlElement{}, [{atom(), term()}]) -> [{atom(), term()}].
user_xml_filter(#xmlText{}, Acc) ->
    Acc;
user_xml_filter(Element, Acc) ->
    case Element#xmlElement.name of
        'Email' ->
            [Content | _] = Element#xmlElement.content,
            case is_record(Content, xmlText) of
                true ->
                    [{email, Content#xmlText.value} | Acc];
                false ->
                    Acc
            end;
        'Name' ->
            [Content | _] = Element#xmlElement.content,
            case is_record(Content, xmlText) of
                true ->
                    [{name, Content#xmlText.value} | Acc];
                false ->
                    Acc
            end;
        'Status' ->
            [Content | _] = Element#xmlElement.content,
            case is_record(Content, xmlText) of
                true ->
                    case Content#xmlText.value of
                        "enabled" ->
                            [{status, enabled} | Acc];
                        "disabled" ->
                            [{status, disabled} | Acc];
                        _ ->
                            Acc
                    end;
                false ->
                    Acc
            end;
        'NewKeySecret' ->
            [Content | _] = Element#xmlElement.content,
            case is_record(Content, xmlText) of
                true ->
                    case Content#xmlText.value of
                        "true" ->
                            [{new_key_secret, true} | Acc];
                        "false" ->
                            [{new_key_secret, false} | Acc];
                        _ ->
                            Acc
                    end;
                false ->
                    Acc
            end;
        _ ->
            Acc
    end.

role_response({ok, Role}, ContentType, RD, Ctx) ->
    Doc = format_role_record(Role, ContentType),
    WrittenRD =
        wrq:set_resp_body(Doc,
                          wrq:set_resp_header("Content-Type", ContentType, RD)),
    {true, WrittenRD, Ctx};
user_response({halt, 200}, ContentType, RD, Ctx) ->
    {{halt, 200}, set_resp_data(ContentType, RD, Ctx), Ctx};
user_response({error, Reason}, _, RD, Ctx) ->
    riak_cs_s3_response:api_error(Reason, RD, Ctx).

-spec format_role_record(role(), string()) -> binary().
format_user_record(User, ?JSON_TYPE) ->
    riak_cs_json:to_json(User);
format_user_record(User, ?XML_TYPE) ->
    riak_cs_xml:to_xml(User).
