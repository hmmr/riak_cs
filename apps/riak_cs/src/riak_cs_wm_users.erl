%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved,
%%               2021-2023 TI Tokyo    All Rights Reserved.
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

-module(riak_cs_wm_users).

-export([init/1,
         service_available/2,
         forbidden/2,
         content_types_provided/2,
         allowed_methods/2,
         options/2,
         produce_json/2,
         produce_xml/2,
         finish_request/2
        ]).

-ignore_xref([init/1,
              service_available/2,
              forbidden/2,
              content_types_provided/2,
              allowed_methods/2,
              options/2,
              produce_json/2,
              produce_xml/2,
              finish_request/2
             ]).

-include("riak_cs.hrl").
-include_lib("kernel/include/logger.hrl").

%% -------------------------------------------------------------------
%% Webmachine callbacks
%% -------------------------------------------------------------------

init(Config) ->
    %% Check if authentication is disabled and
    %% set that in the context.
    AuthBypass = not proplists:get_value(admin_auth_enabled, Config),
    Api = riak_cs_config:api(),
    RespModule = riak_cs_config:response_module(Api),
    {ok, #rcs_web_context{auth_bypass = AuthBypass,
                          api = Api,
                          response_module = RespModule}}.

-spec options(#wm_reqdata{}, #rcs_web_context{}) -> {[{string(), string()}], #wm_reqdata{}, #rcs_web_context{}}.
options(RD, Ctx) ->
    {riak_cs_wm_utils:cors_headers(), RD, Ctx}.

-spec service_available(#wm_reqdata{}, #rcs_web_context{}) -> {true, #wm_reqdata{}, #rcs_web_context{}}.
service_available(RD, Ctx) ->
    riak_cs_wm_utils:service_available(
      wrq:set_resp_headers(riak_cs_wm_utils:cors_headers(), RD), Ctx).

-spec allowed_methods(#wm_reqdata{}, #rcs_web_context{}) -> {[atom()], #wm_reqdata{}, #rcs_web_context{}}.
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'OPTIONS'], RD, Ctx}.


-spec forbidden(#wm_reqdata{}, #rcs_web_context{}) ->
          {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
forbidden(RD, Ctx) ->
    case wrq:method(RD) of
        'OPTIONS' ->
            {false, RD, Ctx};
        _ ->
            forbidden2(RD, Ctx)
    end.
forbidden2(RD, Ctx = #rcs_web_context{auth_bypass = AuthBypass}) ->
    riak_cs_wm_utils:find_and_auth_admin(RD, Ctx, AuthBypass).

content_types_provided(RD, Ctx) ->
    {[{?XML_TYPE, produce_xml}, {?JSON_TYPE, produce_json}], RD, Ctx}.

produce_json(RD, Ctx = #rcs_web_context{riak_client = RcPid}) ->
    Boundary = unique_id(),
    UpdRD = wrq:set_resp_header("Content-Type",
                                "multipart/mixed; boundary="++Boundary,
                                RD),
    StatusQsVal = wrq:get_qs_value("status", RD),
    case StatusQsVal of
        "enabled" ->
            Status = enabled;
        "disabled" ->
            Status = disabled;
        _ ->
            Status = undefined
    end,
    {{stream, {<<>>, fun() -> stream_users(json, RcPid, Boundary, Status) end}}, UpdRD, Ctx}.

produce_xml(RD, Ctx = #rcs_web_context{riak_client = RcPid}) ->
    Boundary = unique_id(),
    UpdRD = wrq:set_resp_header("Content-Type",
                                "multipart/mixed; boundary="++Boundary,
                                RD),
    StatusQsVal = wrq:get_qs_value("status", RD),
    case StatusQsVal of
        "enabled" ->
            Status = enabled;
        "disabled" ->
            Status = disabled;
        _ ->
            Status = undefined
    end,
    {{stream, {<<>>, fun() -> stream_users(xml, RcPid, Boundary, Status) end}}, UpdRD, Ctx}.

finish_request(RD, Ctx) ->
    %% riak_client is still used for streaming response.
    %% So do not close it here.
    {true, RD, Ctx#rcs_web_context{riak_client = undefined}}.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

stream_users(Format, RcPid, Boundary, Status) ->
    {ok, MasterPbc} = riak_cs_riak_client:master_pbc(RcPid),
    case riakc_pb_socket:stream_list_keys(MasterPbc, ?USER_BUCKET) of
        {ok, ReqId} ->
            wait_for_users(Format, RcPid, ReqId, Boundary, Status);
        {error, _Reason} ->
            {<<>>, done}
    end.

wait_for_users(Format, RcPid, ReqId, Boundary, Status) ->
    _ = riak_cs_stats:inflow([riakc, list_users_receive_chunk]),
    StartTime = os:system_time(millisecond),
    receive
        {ReqId, {keys, UserIds}} when UserIds =/= [] ->
            _ = riak_cs_stats:update_with_start(
                  [riakc, list_users_receive_chunk], StartTime),
            FoldFun = user_fold_fun(RcPid, Status),
            Doc = users_doc(lists:foldl(FoldFun, [], UserIds),
                            Format,
                            Boundary),
            {Doc, fun() -> wait_for_users(Format, RcPid, ReqId, Boundary, Status) end};
        {ReqId, done} ->
            _ = riak_cs_stats:update_with_start(
                  [riakc, list_users_receive_chunk], StartTime),
            ok = riak_cs_riak_client:checkin(RcPid),
            {list_to_binary(["\r\n--", Boundary, "--"]), done};
        _Other ->
            wait_for_users(Format, RcPid, ReqId, Boundary, Status)
    end.

%% @doc Compile a multipart entity for a set of user documents.
users_doc(UserDocs, xml, Boundary) ->
    ["\r\n--",
     Boundary,
     "\r\nContent-Type: ", ?XML_TYPE, "\r\n\r\n",
     riak_cs_xml:to_xml({users, UserDocs})];
users_doc(UserDocs, json, Boundary) ->
    ["\r\n--",
     Boundary,
     "\r\nContent-Type: ", ?JSON_TYPE, "\r\n\r\n",
     riak_cs_json:to_json({users, UserDocs})].

%% @doc Return a fold function to retrieve and filter user accounts
user_fold_fun(RcPid, Status) ->
    {ok, Pbc} = riak_cs_riak_client:master_pbc(RcPid),
    fun(Arn, Users) ->
            case riak_cs_iam:get_user(Arn, Pbc) of
                {ok, {User, _}} when User?RCS_USER.status =:= Status;
                                     Status =:= undefined ->
                    [User | Users];
                {ok, _} ->
                    %% Status is defined and does not match the account status
                    Users;
                {error, Reason} ->
                    logger:warning("Failed to fetch user record ~s."
                                   " Reason: ~p", [Arn, Reason]),
                    Users
            end
    end.

unique_id() ->
    Rand = riak_cs_utils:sha(term_to_binary({make_ref(), erlang:timestamp()})),
    <<I:160/integer>> = Rand,
    integer_to_list(I, 36).
