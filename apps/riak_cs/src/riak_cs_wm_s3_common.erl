%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2016 Basho Technologies, Inc.  All Rights Reserved,
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

-module(riak_cs_wm_s3_common).

-export([init/1,
         service_available/2,
         forbidden/2,
         content_types_accepted/2,
         content_types_provided/2,
         generate_etag/2,
         last_modified/2,
         valid_entity_length/2,
         validate_content_checksum/2,
         malformed_request/2,
         options/2,
         to_xml/2,
         to_json/2,
         post_is_create/2,
         create_path/2,
         process_post/2,
         resp_body/2,
         multiple_choices/2,
         add_acl_to_context_then_accept/2,
         accept_body/2,
         produce_body/2,
         allowed_methods/2,
         delete_resource/2,
         finish_request/2]).

-export([default_allowed_methods/0,
         default_stats_prefix/0,
         default_content_types_accepted/2,
         default_content_types_provided/2,
         default_generate_etag/2,
         default_last_modified/2,
         default_finish_request/2,
         default_init/1,
         default_authorize/2,
         default_malformed_request/2,
         default_options/2,
         default_valid_entity_length/2,
         default_validate_content_checksum/2,
         default_delete_resource/2,
         default_anon_ok/0,
         default_produce_body/2,
         default_multiple_choices/2]).

-include("riak_cs.hrl").
-include("oos_api.hrl").
-include_lib("webmachine/include/wm_reqstate.hrl").
-include_lib("kernel/include/logger.hrl").

%% ===================================================================
%% Webmachine callbacks
%% ===================================================================

-spec init([{atom(),term()}]) -> {ok, #rcs_web_context{}}.
init(Config) ->
    Mod = proplists:get_value(submodule, Config),
    %% Check if authentication is disabled and set that in the context.
    AuthBypass = proplists:get_value(auth_bypass, Config),
    AuthModule = proplists:get_value(auth_module, Config),
    Api = riak_cs_config:api(),
    RespModule = riak_cs_config:response_module(Api),
    PolicyModule = proplists:get_value(policy_module, Config),
    Exports = orddict:from_list(Mod:module_info(exports)),
    ExportsFun = exports_fun(Exports),
    StatsPrefix = resource_call(Mod, stats_prefix, [], ExportsFun),
    Ctx = #rcs_web_context{auth_bypass = AuthBypass,
                           auth_module = AuthModule,
                           response_module = RespModule,
                           policy_module = PolicyModule,
                           exports_fun = ExportsFun,
                           stats_prefix = StatsPrefix,
                           submodule = Mod,
                           api = Api},
    resource_call(Mod, init, [Ctx], ExportsFun).

-spec service_available(#wm_reqdata{}, #rcs_web_context{}) -> {boolean(), #wm_reqdata{}, #rcs_web_context{}}.
service_available(RD, #rcs_web_context{rc_pool = undefined} = Ctx) ->
    service_available(RD, Ctx#rcs_web_context{rc_pool = request_pool});
service_available(RD, #rcs_web_context{rc_pool = Pool} = Ctx) ->
    case riak_cs_riak_client:checkout(Pool) of
        {ok, RcPid} ->
            {true, RD, Ctx#rcs_web_context{riak_client = RcPid}};
        {error, _Reason} ->
            {false, RD, Ctx}
    end.

-spec malformed_request(#wm_reqdata{}, #rcs_web_context{}) -> {boolean(), #wm_reqdata{}, #rcs_web_context{}}.
malformed_request(RD, Ctx = #rcs_web_context{submodule = Mod,
                                             exports_fun = ExportsFun,
                                             stats_prefix = StatsPrefix}) ->
    %% Method is used in stats keys, updating inflow should be *after*
    %% allowed_methods assertion.
    _ = update_stats_inflow(RD, StatsPrefix),
    resource_call(Mod, malformed_request, [RD, Ctx], ExportsFun).


-spec options(#wm_reqdata{}, #rcs_web_context{}) -> {[{string(), string()}], #wm_reqdata{}, #rcs_web_context{}}.
options(RD, Ctx = #rcs_web_context{submodule = Mod,
                                   exports_fun = ExportsFun}) ->
    resource_call(Mod, options, [RD, Ctx], ExportsFun).

-spec valid_entity_length(#wm_reqdata{}, #rcs_web_context{}) -> {boolean(), #wm_reqdata{}, #rcs_web_context{}}.
valid_entity_length(RD, #rcs_web_context{submodule = Mod,
                                         exports_fun = ExportsFun} = Ctx) ->
    resource_call(Mod, valid_entity_length, [RD, Ctx], ExportsFun).

-type validate_checksum_response() :: {error, term()} |
                                      {halt, pos_integer()} |
                                      boolean().
-spec validate_content_checksum(#wm_reqdata{}, #rcs_web_context{}) ->
          {validate_checksum_response(), #wm_reqdata{}, #rcs_web_context{}}.
validate_content_checksum(RD, Ctx = #rcs_web_context{submodule = Mod,
                                                     exports_fun = ExportsFun}) ->
    resource_call(Mod, validate_content_checksum, [RD, Ctx], ExportsFun).

-spec forbidden(#wm_reqdata{}, #rcs_web_context{}) -> {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
forbidden(RD, Ctx) ->
    case wrq:method(RD) of
        'OPTIONS' ->
            {false, RD, Ctx};
        _ ->
            forbidden2(RD, Ctx)
    end.
forbidden2(RD, Ctx0 = #rcs_web_context{auth_module = AuthMod,
                                       submodule = Mod,
                                       riak_client = RcPid,
                                       exports_fun = ExportsFun}) ->
    {AuthResult, AnonOk, Ctx} =
        case AuthMod:identify(RD, Ctx0) of
            failed ->
                {{error, no_such_key}, false, Ctx0};
            {failed, Reason} ->
                {{error, Reason}, false, Ctx0};
            {UserKey, AuthData} ->
                case maybe_create_user(
                       riak_cs_user:get_user(UserKey, RcPid),
                       UserKey,
                       Ctx0#rcs_web_context.api,
                       Ctx0#rcs_web_context.auth_module,
                       AuthData,
                       RcPid) of
                    {ok, {User, Obj}} ->
                        {authenticate(
                           User, Obj,
                           RD, Ctx0,
                           AuthData),
                         resource_call(Mod, anon_ok, [], ExportsFun),
                         Ctx0#rcs_web_context{admin_access = riak_cs_user:is_admin(User)}};
                    Error ->
                        {Error,
                         resource_call(Mod, anon_ok, [], ExportsFun), Ctx0}
                end
        end,
    post_authentication(AuthResult, RD, Ctx, AnonOk).

maybe_create_user({ok, {_, _}} = UserResult, _, _, _, _, _) ->
    UserResult;
maybe_create_user({error, NE}, KeyId, oos, _, {UserData, _}, RcPid)
  when NE =:= not_found;
       NE =:= notfound;
       NE =:= no_user_key ->
    {Name, Email, UserId} = UserData,
    {_, Secret} = riak_cs_oos_utils:user_ec2_creds(UserId, KeyId),
    %% Attempt to create a Riak CS user to represent the OS tenant
    _ = riak_cs_user:create_user(Name, Email, KeyId, Secret, #{}),
    riak_cs_user:get_user(KeyId, RcPid);
maybe_create_user({error, NE}, KeyId, aws, riak_cs_keystone_auth, {UserData, _}, RcPid)
  when NE =:= not_found;
       NE =:= notfound;
       NE =:= no_user_key ->
    {Name, Email, UserId} = UserData,
    {_, Secret} = riak_cs_oos_utils:user_ec2_creds(UserId, KeyId),
    %% Attempt to create a Riak CS user to represent the OS tenant
    _ = riak_cs_user:create_user(Name, Email, KeyId, Secret, #{}),
    riak_cs_user:get_user(KeyId, RcPid);
maybe_create_user({error, no_user_key} = Error, _, _, _, _, _) ->
    %% Anonymous access may be authorized by ACL or policy afterwards,
    %% no logging here.
    Error;
maybe_create_user({error, disconnected} = Error, _, _, _, _, RcPid) ->
    {ok, MasterPid} = riak_cs_riak_client:master_pbc(RcPid),
    riak_cs_pbc:check_connection_status(MasterPid, maybe_create_user),
    Error;
maybe_create_user({error, _Reason} = Error, _, _Api, _, _, _) ->
    Error.

%% @doc Get the list of methods a resource supports.
-spec allowed_methods(#wm_reqdata{}, #rcs_web_context{}) -> {[atom()], #wm_reqdata{}, #rcs_web_context{}}.
allowed_methods(RD, Ctx = #rcs_web_context{submodule = Mod,
                                           exports_fun = ExportsFun}) ->
    Methods = resource_call(Mod,
                            allowed_methods,
                            [],
                            ExportsFun),
    {Methods, RD, Ctx}.

-spec content_types_accepted(#wm_reqdata{}, #rcs_web_context{}) -> {[{string(), atom()}], #wm_reqdata{}, #rcs_web_context{}}.
content_types_accepted(RD, Ctx = #rcs_web_context{submodule = Mod,
                                                  exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  content_types_accepted,
                  [RD,Ctx],
                  ExportsFun).

-spec content_types_provided(#wm_reqdata{}, #rcs_web_context{}) -> {[{string(), atom()}], #wm_reqdata{}, #rcs_web_context{}}.
content_types_provided(RD, Ctx = #rcs_web_context{submodule = Mod,
                                                  exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  content_types_provided,
                  [RD,Ctx],
                  ExportsFun).

-spec generate_etag(#wm_reqdata{}, #rcs_web_context{}) -> {string(), #wm_reqdata{}, #rcs_web_context{}}.
generate_etag(RD, Ctx = #rcs_web_context{submodule = Mod,
                                         exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  generate_etag,
                  [RD,Ctx],
                  ExportsFun).

-spec last_modified(#wm_reqdata{}, #rcs_web_context{}) -> {calendar:datetime(), #wm_reqdata{}, #rcs_web_context{}}.
last_modified(RD, Ctx = #rcs_web_context{submodule = Mod,
                                         exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  last_modified,
                  [RD,Ctx],
                  ExportsFun).

-spec delete_resource(#wm_reqdata{}, #rcs_web_context{}) -> {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
delete_resource(RD, Ctx = #rcs_web_context{submodule = Mod,
                                           exports_fun = ExportsFun}) ->
    %% TODO: add dt_wm_return from subresource?
    resource_call(Mod,
                  delete_resource,
                  [RD,Ctx],
                  ExportsFun).

-spec to_xml(#wm_reqdata{}, #rcs_web_context{}) ->
          {binary() | {'halt', non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
to_xml(RD, Ctx = #rcs_web_context{submodule = Mod,
                                  exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  to_xml,
                  [RD, Ctx],
                  ExportsFun).

-spec to_json(#wm_reqdata{}, #rcs_web_context{}) ->
          {binary() | {'halt', non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
to_json(RD, Ctx = #rcs_web_context{submodule = Mod,
                                   exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  to_json,
                  [RD, Ctx],
                  ExportsFun(to_json)).

post_is_create(RD, Ctx = #rcs_web_context{submodule = Mod,
                                          exports_fun = ExportsFun}) ->
    resource_call(Mod, post_is_create, [RD, Ctx], ExportsFun).

create_path(RD, #rcs_web_context{submodule = Mod,
                                 exports_fun = ExportsFun} = Ctx) ->
    resource_call(Mod, create_path, [RD, Ctx], ExportsFun).

process_post(RD, #rcs_web_context{submodule = Mod,
                                  exports_fun = ExportsFun} = Ctx) ->
    resource_call(Mod, process_post, [RD, Ctx], ExportsFun).

resp_body(RD, #rcs_web_context{submodule = Mod,
                               exports_fun = ExportsFun} = Ctx) ->
    resource_call(Mod, resp_body, [RD, Ctx], ExportsFun).

multiple_choices(RD, #rcs_web_context{submodule = Mod,
                                      exports_fun = ExportsFun} = Ctx) ->
    try
        resource_call(Mod, multiple_choices, [RD, Ctx], ExportsFun)
    catch _:_ ->
            {false, RD, Ctx}
    end.

%% @doc Add an ACL (or default ACL) to the context, parsed from headers. If
%% parsing the headers fails, halt the request.
add_acl_to_context_then_accept(RD, Ctx) ->
    case riak_cs_wm_utils:maybe_update_context_with_acl_from_headers(RD, Ctx) of
        {ok, ContextWithAcl} ->
            accept_body(RD, ContextWithAcl);
        {error, HaltResponse} ->
            HaltResponse
    end.

-spec accept_body(#wm_reqdata{}, #rcs_web_context{}) ->
          {boolean() | {'halt', non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
accept_body(RD, Ctx = #rcs_web_context{submodule = Mod,
                                       exports_fun = ExportsFun}) ->
    resource_call(Mod,
                  accept_body,
                  [RD, Ctx],
                  ExportsFun).
    %% TODO: extract response code and add to ints field

-spec produce_body(#wm_reqdata{}, #rcs_web_context{}) ->
          {iolist()|binary(), #wm_reqdata{}, #rcs_web_context{}} |
          {{known_length_stream, non_neg_integer(), {<<>>, function()}}, #wm_reqdata{}, #rcs_web_context{}}.
produce_body(RD, Ctx = #rcs_web_context{submodule = Mod,
                                        exports_fun = ExportsFun}) ->
    %% TODO: add dt_wm_return w/ content length
    resource_call(Mod,
                  produce_body,
                  [RD, Ctx],
                  ExportsFun).

-spec finish_request(#wm_reqdata{}, #rcs_web_context{}) -> {boolean(), #wm_reqdata{}, #rcs_web_context{}}.
finish_request(RD, Ctx = #rcs_web_context{riak_client = RcPid,
                                          auto_rc_close = AutoRcClose,
                                          submodule = Mod,
                                          exports_fun = ExportsFun})
  when RcPid =:= undefined orelse AutoRcClose =:= false ->
    Res = resource_call(Mod,
                        finish_request,
                        [RD, Ctx],
                        ExportsFun),
    update_stats(RD, Ctx),
    Res;
finish_request(RD, Ctx0 = #rcs_web_context{riak_client = RcPid,
                                           rc_pool = Pool,
                                           submodule = Mod,
                                           exports_fun = ExportsFun}) ->
    riak_cs_riak_client:checkin(Pool, RcPid),
    Ctx = Ctx0#rcs_web_context{riak_client=undefined},
    Res = resource_call(Mod,
                        finish_request,
                        [RD, Ctx],
                        ExportsFun),
    update_stats(RD, Ctx),
    Res.

%% ===================================================================
%% Helper functions
%% ===================================================================

-spec authorize(#wm_reqdata{}, #rcs_web_context{}) -> {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
authorize(RD, Ctx = #rcs_web_context{submodule = Mod,
                                     exports_fun = ExportsFun}) ->
    resource_call(Mod, authorize, [RD,Ctx], ExportsFun).

-spec authenticate(rcs_user(), riakc_obj:riakc_obj(), term(), term(), term()) ->
          {ok, rcs_user(), riakc_obj:riakc_obj()} | {error, bad_auth}.
authenticate(User, UserObj, RD, Ctx = #rcs_web_context{auth_module = AuthMod}, AuthData)
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

-spec exports_fun(orddict:orddict()) -> function().
exports_fun(Exports) ->
    fun(Function) ->
            orddict:is_key(Function, Exports)
    end.


resource_call(Mod, Fun, Args, true) ->
    erlang:apply(Mod, Fun, Args);
resource_call(_Mod, Fun, Args, false) ->
    erlang:apply(?MODULE, default(Fun), Args);
resource_call(Mod, Fun, Args, ExportsFun) ->
    resource_call(Mod, Fun, Args, ExportsFun(Fun)).


post_authentication(AuthResult, RD, Ctx, AnonOk) ->
    post_authentication(AuthResult, RD, Ctx, fun authorize/2, AnonOk).

post_authentication({ok, User, UserObj}, RD, Ctx, Authorize, _) ->
    %% given keyid and signature matched, proceed
    Authorize(RD, Ctx#rcs_web_context{user = User,
                                      user_object = UserObj});
post_authentication({error, no_user_key}, RD, Ctx, Authorize, true) ->
    %% no keyid was given, proceed anonymously
    ?LOG_DEBUG("No user key"),
    Authorize(RD, Ctx);
post_authentication({error, no_user_key}, RD, Ctx, _, false) ->
    %% no keyid was given, deny access
    ?LOG_DEBUG("No user key, deny"),
    riak_cs_wm_utils:deny_access(RD, Ctx);
post_authentication({error, bad_auth}, RD, Ctx, _, _) ->
    %% given keyid was found, but signature didn't match
    ?LOG_DEBUG("bad_auth"),
    riak_cs_wm_utils:deny_access(RD, Ctx);
post_authentication({error, reqtime_tooskewed} = Error, RD,
                    #rcs_web_context{response_module = ResponseMod} = Ctx, _, _) ->
    ?LOG_DEBUG("reqtime_tooskewed"),
    ResponseMod:api_error(Error, RD, Ctx);
post_authentication({error, {auth_not_supported, AuthType}}, RD,
                    Ctx = #rcs_web_context{response_module = ResponseMod} = Ctx, _, _) ->
    ?LOG_DEBUG("auth_not_supported: ~s", [AuthType]),
    ResponseMod:api_error({auth_not_supported, AuthType}, RD, Ctx);
post_authentication({error, notfound}, RD, Ctx, _, _) ->
    ?LOG_DEBUG("User does not exist"),
    riak_cs_wm_utils:deny_invalid_key(RD, Ctx);
post_authentication({error, Reason}, RD,
                    #rcs_web_context{response_module = ResponseMod} = Ctx, _, _) ->
    %% Lookup failed, basically due to disconnected stuff
    ?LOG_DEBUG("Authentication error: ~p", [Reason]),
    ResponseMod:api_error(Reason, RD, Ctx).

update_stats_inflow(_RD, undefined = _StatsPrefix) ->
    ok;
update_stats_inflow(_RD, no_stats = _StatsPrefix) ->
    ok;
update_stats_inflow(RD, StatsPrefix) ->
    Method = riak_cs_wm_utils:lower_case_method(wrq:method(RD)),
    Key = [StatsPrefix, Method],
    riak_cs_stats:inflow(Key).

update_stats(_RD, #rcs_web_context{stats_key = no_stats}) ->
    ok;
update_stats(_RD, #rcs_web_context{stats_prefix = no_stats}) ->
    ok;
update_stats(RD, #rcs_web_context{start_time = StartTime,
                                  stats_prefix = StatsPrefix,
                                  stats_key = StatsKey}) ->
    update_stats(StartTime,
                 wrq:response_code(RD),
                 StatsPrefix,
                 riak_cs_wm_utils:lower_case_method(wrq:method(RD)),
                 StatsKey).

update_stats(StartTime, Code, StatsPrefix, Method, StatsKey0) ->
    StatsKey = case StatsKey0 of
                   prefix_and_method -> [StatsPrefix, Method];
                   _  -> StatsKey0
               end,
    case Code of
        405 ->
            %% Method Not Allowed: don't update stats because unallowed
            %% mothod may lead to notfound warning in updating stats
            ok;
        Success when is_integer(Success) andalso Success < 400 ->
            riak_cs_stats:update_with_start(StatsKey, StartTime);
        _Error ->
            riak_cs_stats:update_error_with_start(StatsKey, StartTime)
    end.

%% ===================================================================
%% Resource function defaults
%% ===================================================================

default(init) ->
    default_init;
default(stats_prefix) ->
    default_stats_prefix;
default(allowed_methods) ->
    default_allowed_methods;
default(content_types_accepted) ->
    default_content_types_accepted;
default(content_types_provided) ->
    default_content_types_provided;
default(generate_etag) ->
    default_generate_etag;
default(last_modified) ->
    default_last_modified;
default(malformed_request) ->
    default_malformed_request;
default(options) ->
    default_options;
default(valid_entity_length) ->
    default_valid_entity_length;
default(validate_content_checksum) ->
    default_validate_content_checksum;
default(delete_resource) ->
    default_delete_resource;
default(authorize) ->
    default_authorize;
default(finish_request) ->
    default_finish_request;
default(anon_ok) ->
    default_anon_ok;
default(produce_body) ->
    default_produce_body;
default(multiple_choices) ->
    default_multiple_choices;
default(_) ->
    undefined.

default_init(Ctx) ->
    {ok, Ctx}.

default_stats_prefix() ->
    no_stats.

default_malformed_request(RD, Ctx) ->
    {false, RD, Ctx}.

default_options(RD, Ctx) ->
    {[{"Access-Control-Allow-Origin", "*"}], RD, Ctx}.

default_valid_entity_length(RD, Ctx) ->
    {true, RD, Ctx}.

default_validate_content_checksum(RD, Ctx) ->
    {true, RD, Ctx}.

default_content_types_accepted(RD, Ctx) ->
    {[], RD, Ctx}.

-spec default_content_types_provided(#wm_reqdata{}, #rcs_web_context{}) ->
          {[{string(), atom()}],
           #wm_reqdata{},
           #rcs_web_context{}}.
default_content_types_provided(RD, Ctx = #rcs_web_context{api = oos}) ->
    {[{"text/plain", produce_body}], RD, Ctx};
default_content_types_provided(RD, Ctx) ->
    {[{"application/xml", produce_body}], RD, Ctx}.

default_generate_etag(RD, Ctx) ->
    {undefined, RD, Ctx}.

default_last_modified(RD, Ctx) ->
    {undefined, RD, Ctx}.

default_delete_resource(RD, Ctx) ->
    {false, RD, Ctx}.

default_allowed_methods() ->
    ['OPTIONS'].

default_finish_request(RD, Ctx) ->
    {true, RD, Ctx}.

default_anon_ok() ->
    true.

default_produce_body(RD, Ctx = #rcs_web_context{submodule = Mod,
                                                response_module = ResponseMod,
                                                exports_fun = ExportsFun}) ->
    try
        ResponseMod:respond(
          resource_call(Mod, api_request, [RD, Ctx], ExportsFun),
          RD,
          Ctx)
    catch error:{badmatch, {error, Reason}} ->
            ResponseMod:api_error(Reason, RD, Ctx)
    end.

%% @doc this function will be called by `post_authenticate/2' if the user successfully
%% authenticates and the submodule does not provide an implementation
%% of authorize/2. The default implementation does not perform any authorization
%% and simply returns false to signify the request is not fobidden
-spec default_authorize(term(), term()) -> {false, term(), term()}.
default_authorize(RD, Ctx) ->
    {false, RD, Ctx}.

default_multiple_choices(RD, Ctx) ->
    {false, RD, Ctx}.
