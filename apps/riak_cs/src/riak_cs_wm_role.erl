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
         content_types_provided/2,
         content_types_accepted/2,
         authorize/2,
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
              content_types_provided/2,
              content_types_accepted/2,
              authorize/2,
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
-include_lib("kernel/include/logger.hrl").

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

-spec content_types_accepted(#wm_reqdata{}, #rcs_context{}) ->
    {[{string(), accept_xml}], #wm_reqdata{}, #rcs_context{}}.
content_types_accepted(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"content_types_accepted">>),
    {[{?XML_TYPE, accept_xml}, {?JSON_TYPE, accept_json}], RD, Ctx}.

-spec content_types_provided(#wm_reqdata{}, #rcs_context{}) ->
    {[{string(), produce_xml}], #wm_reqdata{}, #rcs_context{}}.
content_types_provided(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"content_types_provided">>),
    {[{?XML_TYPE, produce_xml}, {?JSON_TYPE, produce_json}], RD, Ctx}.


-spec authorize(#wm_reqdata{}, #rcs_context{}) ->
    {boolean() | {halt, term()}, #wm_reqdata{}, #rcs_context{}}.
authorize(RD, Ctx) ->
    Method = wrq:method(RD),
    riak_cs_wm_utils:role_access_authorize_helper(Method, RD, Ctx).


post_is_create(RD, Ctx) -> {true, RD, Ctx}.

-spec accept_json(#wm_reqdata{}, #rcs_context{}) ->
    {boolean() | {halt, term()}, term(), term()}.
accept_json(RD, Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"accept_json">>),
    Specs =
        lists:foldl(fun role_json_filter/2, [], riak_cs_json:from_json(wrq:req_body(RD))),
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

-spec produce_json(#wm_reqdata{}, #rcs_context{}) ->
    {string(), #wm_reqdata{}, #rcs_context{}}.
produce_json(RD, Ctx = #rcs_context{riak_client = RcPid}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"produce_json">>),
    RoleId = list_to_binary(wrq:path_info(role, RD)),
    Body = riak_cs_json:to_json(
             riak_cs_role:get_role(RoleId, RcPid)),
    Etag = etag(Body),
    RD2 = wrq:set_resp_header("ETag", Etag, RD),
    {Body, RD2, Ctx}.

-spec produce_xml(#wm_reqdata{}, #rcs_context{}) ->
    {string(), #wm_reqdata{}, #rcs_context{}}.
produce_xml(RD, #rcs_context{riak_client = RcPid}=Ctx) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"produce_xml">>),
    RoleId = list_to_binary(wrq:path_info(role, RD)),
    Body = riak_cs_xml:to_xml(
             riak_cs_role:get_role(RoleId, RcPid)),
    Etag = etag(Body),
    RD2 = wrq:set_resp_header("ETag", Etag, RD),
    {Body, RD2, Ctx}.

finish_request(RD, Ctx=#rcs_context{riak_client = undefined}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"finish_request">>, [0], []),
    {true, RD, Ctx};
finish_request(RD, Ctx=#rcs_context{riak_client=RcPid}) ->
    riak_cs_dtrace:dt_wm_entry(?MODULE, <<"finish_request">>, [1], []),
    riak_cs_riak_client:checkin(RcPid),
    riak_cs_dtrace:dt_wm_return(?MODULE, <<"finish_request">>, [1], []),
    {true, RD, Ctx#rcs_context{riak_client = undefined}}.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

%% @doc Calculate the etag of a response body
etag(Body) ->
        riak_cs_utils:etag_from_binary(riak_cs_utils:md5(Body)).

role_json_filter({ItemKey, ItemValue}, Acc) ->
    case ItemKey of
        <<"AssumeRolePolicyDocument">> ->
            [{assume_role_policy_document, ItemValue} | Acc];
        <<"Description">> ->
            [{description, ItemValue} | Acc];
        <<"MaxSessionDuration">> ->
            [{max_session_duration, ItemValue} | Acc];
        <<"Path">> ->
            [{path, ItemValue} | Acc];
        <<"PermissionsBoundary">> ->
            [{path, ItemValue} | Acc];
        <<"RoleName">> ->
            [{path, ItemValue} | Acc];
        <<"Tags">> ->
            [{path, ItemValue} | Acc];
        _ ->
            Acc
    end.

role_xml_filter(#xmlText{}, Acc) ->
    Acc;
role_xml_filter(Element, Acc) ->
    case Element#xmlElement.name of
        'AssumeRolePolicyDocument' ->
            extract_to_acc(Element, assume_role_policy_document, Acc);
        'Description' ->
            extract_to_acc(Element, description, Acc);
        'MaxSessionDuration' ->
            extract_to_acc(Element, max_session_duration, Acc);
        'Path' ->
            extract_to_acc(Element, path, Acc);
        'PermissionsBoundary' ->
            extract_to_acc(Element, permissions_boundary, Acc);
        'RoleName' ->
            extract_to_acc(Element, role_name, Acc);
        MaybeTag ->
            maybe_extract_tag_to_acc(Element, MaybeTag, Acc)
    end.

extract_to_acc(E, N, Q) ->
    [A | _] = E#xmlElement.content,
    case is_record(A, xmlText) of
        true ->
            [{N, A#xmlText.value} | Q];
        false ->
            Q
    end.

maybe_extract_tag_to_acc(E, Tag, Acc) ->
    ?LOG_DEBUG("STUB pretend adding tag ~p from element ~p", [Tag, E]),
    Acc.

role_response({ok, Role}, ContentType, RD, Ctx) ->
    Doc = format_role_record(Role, ContentType),
    WrittenRD =
        wrq:set_resp_body(Doc,
                          wrq:set_resp_header("Content-Type", ContentType, RD)),
    {true, WrittenRD, Ctx};
role_response({error, Reason}, _, RD, Ctx) ->
    riak_cs_s3_response:api_error(Reason, RD, Ctx).

format_role_record(Role, ?JSON_TYPE) ->
    riak_cs_json:to_json(Role);
format_role_record(Role, ?XML_TYPE) ->
    riak_cs_xml:to_xml(Role).
