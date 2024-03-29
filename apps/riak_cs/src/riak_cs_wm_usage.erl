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

%% @doc Webmachine resource for serving usage stats.
%%
%% `GET /usage/USER_KEY?s=STARTTIME&e=ENDTIME&a=ACCESS&b=STORAGE'
%%
%% `s3cmd get s3://usage/USER_KEY/ab/STARTTIME/ENDTIME'
%%
%% Default `s' is the beginning of the previous period.  If no `e' is
%% given, the return only includes data for the period containing `s'.
%%
%% The `a' and `b' parameters default to `false'. Set `a' to `true' to
%% get access usage stats.  Set `b' to `true' to get storage usage
%% stats.  Including each in the s3cmd enables that stat, excluding it
%% disables it.
%%
%% The `s3cmd' variant also supports `x' and `j' in the path segment
%% with the `a' and `b'.  Including `x' gets the results in XML
%% format; `j' gets them in JSON.  Default is JSON.
%%
%% This service is only available if a connection to Riak can be made.
%%
%% This resource only exists if the user named by the key exists.
%%
%% Authorization is done the same way as all other authorized
%% resources: with the `Authorization' header, hashed with S3 keys,
%% etc.
%%
%% JSON response looks like:
%% ```
%% {"Access":{
%%            "Nodes":[{"Node":"riak-cs@127.0.0.1",
%%                      "Samples":[{"StartTime":"20120113T194510Z",
%%                                  "EndTime":"20120113T194520Z",
%%                                  "KeyRead":{"Count":1,
%%                                             "BytesOut":22},
%%                                  "KeyWrite":{"Count":2,
%%                                              "BytesIn":44}},
%%                                 {"StartTime":"20120113T194520Z",
%%                                  "EndTime":"20120113T194530Z",
%%                                  "KeyRead":{"Count":3,
%%                                             "BytesOut":66},
%%                                  "KeyWrite":{"Count":4,
%%                                              "BytesIn":88}}
%%                                ]}],
%%            "Errors":[]
%%           },
%%  "Storage":"not_requested"}
%% '''
%%
%% XML response looks like:
%% ```
%% <?xml version="1.0" encoding="UTF-8"?>
%%   <Usage>
%%     <Access>
%%       <Nodes>
%%         <Node name="riak-cs@127.0.0.1">
%%           <Sample StartTime="20120113T194510Z" EndTime="20120113T194520Z">
%%             <Operation type="KeyRead">
%%               <Count>1</Count>
%%               <BytesOut>22</BytesOut>
%%             </Operation>
%%             <Operation type="KeyWrite">
%%               <Count>2</Count>
%%               <BytesIn>44</BytesIn>
%%             </Operation>
%%           </Sample>
%%           <Sample StartTime="20120113T194520Z" EndTime="20120113T194530Z">
%%             <Operation type="KeyRead">
%%               <Count>3</Count>
%%               <BytesOut>66</BytesOut>
%%             </Operation>
%%             <Operation type="KeyWrite">
%%               <Count>4</Count>
%%               <BytesIn>88</BytesIn>
%%             </Operation>
%%           </Sample>
%%         </Node>
%%       </Nodes>
%%       <Errors/>
%%     </Access>
%%   <Storage>not_requested</Storage>
%% </Usage>
%% '''

-module(riak_cs_wm_usage).

-export([init/1,
         service_available/2,
         malformed_request/2,
         resource_exists/2,
         content_types_provided/2,
         generate_etag/2,
         forbidden/2,
         produce_json/2,
         produce_xml/2,
         finish_request/2
        ]).

-ignore_xref([init/1,
              service_available/2,
              malformed_request/2,
              resource_exists/2,
              content_types_provided/2,
              generate_etag/2,
              forbidden/2,
              produce_json/2,
              produce_xml/2,
              finish_request/2
             ]).

-on_load(on_load/0).

-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-endif.

-include("rts.hrl").
-include("riak_cs.hrl").
-include_lib("kernel/include/logger.hrl").

%% Keys used in output - defined here to help keep JSON and XML output
%% as similar as possible.
-define(KEY_USAGE, 'Usage').
-define(KEY_ACCESS, 'Access').
-define(KEY_STORAGE, 'Storage').
-define(KEY_NODE, 'Node').
-define(KEY_NODES, 'Nodes').
-define(KEY_SAMPLE,  'Sample').
-define(KEY_SAMPLES, 'Samples').
-define(KEY_OPERATION, 'Operation').
-define(KEY_TYPE, 'type').
-define(KEY_BUCKET, 'Bucket').
-define(KEY_NAME, 'name').
-define(KEY_ERROR, 'Error').
-define(KEY_ERRORS, 'Errors').
-define(KEY_REASON, 'Reason').
-define(KEY_MESSAGE, 'Message').
-define(ATTR_START, 'StartTime').
-define(ATTR_END, 'EndTime').

-record(local_context, { start_time :: undefined | calendar:datetime()
                       , end_time :: undefined | calendar:datetime()
                       , body :: undefined | iodata()
                       , etag :: undefined | iolist()
                       }
       ).

on_load() ->
    %% put atoms into atom table, for binary_to_existing_atom/2 in xml_name/1
    ?SUPPORTED_USAGE_FIELD = lists:map(fun(Bin) ->
                                               binary_to_existing_atom(Bin, latin1)
                                       end, ?SUPPORTED_USAGE_FIELD_BIN),
    ok.

init(Config) ->
    AuthBypass = not proplists:get_value(admin_auth_enabled, Config),
    {ok, #rcs_web_context{auth_bypass = AuthBypass,
                          local_context = #local_context{}}}.

service_available(RD, Ctx) ->
    case riak_cs_riak_client:checkout() of
        {ok, RcPid} ->
            {true, RD, Ctx#rcs_web_context{riak_client = RcPid}};
        {error, _} ->
            {false, error_msg(RD, <<"Usage database connection failed">>), Ctx}
    end.

malformed_request(RD, #rcs_web_context{local_context = LCtx0} = Ctx) ->
    case parse_start_time(RD) of
        {ok, Start} ->
            case parse_end_time(RD, Start) of
                {ok, End} ->
                    case too_many_periods(Start, End) of
                        true ->
                            {true,
                             error_msg(RD, <<"Too much time requested">>),
                             Ctx};
                        false ->
                            LCtx = LCtx0#local_context{start_time = lists:min([Start, End]),
                                                       end_time = lists:max([Start, End])},
                            {false, RD,
                             Ctx#rcs_web_context{local_context = LCtx}}
                    end;
                error ->
                    {true, error_msg(RD, <<"Invalid end-time format">>), Ctx}
            end;
        error ->
            {true, error_msg(RD, <<"Invalid start-time format">>), Ctx}
    end.

resource_exists(RD, #rcs_web_context{riak_client = RcPid} = Ctx) ->
    case riak_cs_user:get_user(user_key(RD), RcPid) of
        {ok, {User, _UserObj}} ->
            {true, RD, Ctx#rcs_web_context{user = User}};
        {error, _} ->
            {false, error_msg(RD, <<"Unknown user">>), Ctx}
    end.

content_types_provided(RD, Ctx) ->
    Types = case {true_param(RD, "j"), true_param(RD, "x")} of
                {true,_} -> [{?JSON_TYPE, produce_json}];
                {_,true} -> [{?XML_TYPE, produce_xml}];
                {_,_} -> [{?JSON_TYPE, produce_json},
                          {?XML_TYPE, produce_xml}]
            end,
    {Types, RD, Ctx}.

generate_etag(RD, #rcs_web_context{local_context = #local_context{etag = undefined} = LCtx} = Ctx) ->
    case content_types_provided(RD, Ctx) of
        {[{_Type, Producer}], _, _} -> ok;
        {Choices, _, _} ->
            Accept = wrq:get_req_header("Accept", RD),
            ChosenType = webmachine_util:choose_media_type(
                           [ Type || {Type, _} <- Choices ],
                           Accept),
            case [ P || {T, P} <- Choices, T == ChosenType ] of
                [] -> Producer = element(2, hd(Choices));
                [Producer|_] -> ok
            end
    end,
    {Body, NewRD, NewCtx} = ?MODULE:Producer(RD, Ctx),
    Etag = riak_cs_utils:etag_from_binary_no_quotes(riak_cs_utils:md5(Body)),
    {Etag, NewRD, NewCtx#rcs_web_context{local_context = LCtx#local_context{etag = Etag}}};
generate_etag(RD, #rcs_web_context{local_context = #local_context{etag = Etag}} = Ctx) ->
    {Etag, RD, Ctx}.

forbidden(RD, #rcs_web_context{auth_bypass = AuthBypass,
                               riak_client = RcPid} = Ctx) ->
    BogusContext = #rcs_web_context{auth_bypass = AuthBypass,
                                    riak_client = RcPid},
    Next = fun(NewRD, #rcs_web_context{user = User}) ->
                   forbidden(NewRD, Ctx, User, AuthBypass)
           end,
    Conv2Ctx = fun(_) -> Ctx end,
    riak_cs_wm_utils:find_and_auth_user(RD, BogusContext, Next, Conv2Ctx, AuthBypass).

forbidden(RD, Ctx, _, true) ->
    %% Treat AuthBypass=true as same as admin access
    {false, RD, Ctx#rcs_web_context{admin_access = true}};
forbidden(RD, Ctx, undefined, false) ->
    %% anonymous access disallowed
    riak_cs_wm_utils:deny_access(RD, Ctx);
forbidden(RD, Ctx, User, false) ->
    case riak_cs_config:admin_creds() of
        {ok, {Admin, _}} when Admin == User?RCS_USER.key_id ->
            %% admin can access anyone's stats
            {false, RD, Ctx#rcs_web_context{admin_access = true}};
        _ ->
            case user_key(RD) == User?RCS_USER.key_id of
                true ->
                    %% user is accessing own stats
                    AccessRD = riak_cs_access_log_handler:set_user(User, RD),
                    {false, AccessRD, Ctx};
                false ->
                    %% no one else is allowed
                    riak_cs_wm_utils:deny_access(RD, Ctx)
            end
    end.

finish_request(RD, #rcs_web_context{riak_client = undefined} = Ctx) ->
    {true, RD, Ctx};
finish_request(RD, #rcs_web_context{riak_client = RcPid} = Ctx) ->
    riak_cs_riak_client:checkin(RcPid),
    {true, RD, Ctx#rcs_web_context{riak_client = undefined}}.

%% JSON Production %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
produce_json(RD, #rcs_web_context{local_context = #local_context{body = undefined} = LCtx0} = Ctx) ->
    Access = maybe_access(RD, Ctx),
    Storage = maybe_storage(RD, Ctx),
    MJ = [{?KEY_ACCESS, json_access(Access)},
          {?KEY_STORAGE, json_storage(Storage)}],
    Body = jsx:encode(MJ),
    {Body, RD, Ctx#rcs_web_context{local_context = LCtx0#local_context{body = Body}}};
produce_json(RD, #rcs_web_context{local_context = #local_context{body = Body}} = Ctx) ->
    {Body, RD, Ctx}.

json_access(Msg) when is_atom(Msg) ->
    Msg;
json_access({Access, Errors}) ->
    Nodes = [[{?KEY_NODE, Node}, {?KEY_SAMPLES, [S || S <- Samples]}]
             || {Node, Samples} <- Access],
    Errs = [ json_sample_error(E) || E <- Errors ],
    [{?KEY_NODES, Nodes},
     {?KEY_ERRORS, Errs}].

json_sample_error({{Start, End}, Reason}) ->
    [{?START_TIME, rts:iso8601(Start)},
     {?END_TIME, rts:iso8601(End)},
     {?KEY_REASON, json_reason(Reason)}].

json_reason(Reason) ->
    if is_atom(Reason) -> atom_to_binary(Reason, latin1);
       is_binary(Reason) -> Reason;
       true -> list_to_binary(io_lib:format("~p", [Reason]))
    end.

json_storage(Msg) when is_atom(Msg) ->
    Msg;
json_storage({Storage, Errors}) ->
    [{?KEY_SAMPLES, Storage},
     {?KEY_ERRORS, [json_sample_error(E) || E <- Errors]}].

%% XML Production %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
produce_xml(RD, #rcs_web_context{local_context = #local_context{body = undefined} = LCtx} = Ctx) ->
    Access = maybe_access(RD, Ctx),
    Storage = maybe_storage(RD, Ctx),
    Doc = [{?KEY_USAGE, [{?KEY_ACCESS, xml_access(Access)},
                         {?KEY_STORAGE, xml_storage(Storage)}]}],
    Body = riak_cs_xml:to_xml(Doc),
    {Body, RD, Ctx#rcs_web_context{local_context = LCtx#local_context{body = Body}}};
produce_xml(RD, #rcs_web_context{local_context = #local_context{body = Body}} = Ctx) ->
    {Body, RD, Ctx}.

xml_access(Msg) when is_atom(Msg) ->
    [atom_to_list(Msg)];
xml_access({Access, Errors}) ->
    Nodes = [{?KEY_NODE, [{name, Node}],
              [xml_sample(S, ?KEY_OPERATION, ?KEY_TYPE) || S <- Samples]}
             || {Node, Samples} <- Access],
    Errs = [ xml_sample_error(E, ?KEY_OPERATION, ?KEY_TYPE)
             || E <- Errors ],
    [{?KEY_NODES, Nodes},
     {?KEY_ERRORS, Errs}].

xml_sample(Sample, SubType, TypeLabel) ->
    {value, {?START_TIME,S}, SampleS} =
        lists:keytake(?START_TIME, 1, Sample),
    {value, {?END_TIME,E}, Rest} =
        lists:keytake(?END_TIME, 1, SampleS),

    {?KEY_SAMPLE, [{xml_name(?START_TIME), S}, {xml_name(?END_TIME), E}],
     [{SubType, [{TypeLabel, OpName}],
       [{xml_name(K), [mochinum:digits(V)]} || {K, V} <- Stats]}
      || {OpName, Stats} <- Rest ]}.

xml_sample_error({{Start, End}, Reason}, SubType, TypeLabel) ->
    %% cheat to make errors structured exactly like samples
    FakeSample = [{?START_TIME, rts:iso8601(Start)},
                  {?END_TIME, rts:iso8601(End)}],
    {Tag, Props, Contents} = xml_sample(FakeSample, SubType, TypeLabel),

    XMLReason = xml_reason(Reason),
    {Tag, Props, [{?KEY_REASON, [XMLReason]}|Contents]}.

%% @doc JSON deserializes with keys as binaries, but xmerl requires
%% tag names to be atoms.
xml_name(?START_TIME) -> ?ATTR_START;
xml_name(?END_TIME) -> ?ATTR_END;
xml_name(UsageFieldName) ->
    true = lists:member(UsageFieldName, ?SUPPORTED_USAGE_FIELD_BIN),
    binary_to_existing_atom(UsageFieldName, latin1).

xml_reason(Reason) ->
    [if is_atom(Reason) -> atom_to_binary(Reason, latin1);
       is_binary(Reason) -> Reason;
       true -> io_lib:format("~p", [Reason])
     end].

xml_storage(Msg) when is_atom(Msg) ->
    [atom_to_list(Msg)];
xml_storage({Storage, Errors}) ->
    [{?KEY_SAMPLES, [xml_sample(S, ?KEY_BUCKET, ?KEY_NAME)
                     || S <- Storage]},
     {?KEY_ERRORS, [xml_sample_error(E, ?KEY_BUCKET, ?KEY_NAME)
                    || E <- Errors ]}].

%% Internals %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
user_key(RD) ->
    case path_tokens(RD) of
        [KeyId|_] -> list_to_binary(mochiweb_util:unquote(KeyId));
        _         -> <<>>
    end.

maybe_access(RD, Ctx) ->
    usage_if(RD, Ctx, "a", riak_cs_access).

maybe_storage(RD, Ctx) ->
    usage_if(RD, Ctx, "b", riak_cs_storage).

usage_if(RD, #rcs_web_context{riak_client = RcPid,
                              admin_access = AdminAccess,
                              user = CallingUser,
                              local_context = #local_context{start_time = Start,
                                                             end_time = End}},
         QParam, Module) ->
    case true_param(RD, QParam) of
        true ->
            TargetKeyId = user_key(RD),
            case CallingUser of
                ?RCS_USER{arn = Arn,
                          key_id = TargetKeyId} ->
                    Module:get_usage(RcPid, Arn, AdminAccess, Start, End);
                _ ->
                    case riak_cs_user:get_user(TargetKeyId, RcPid) of
                        {ok, {?RCS_USER{arn = Arn}, _}} ->
                            Module:get_usage(RcPid, Arn, AdminAccess, Start, End);
                        _ ->
                            logger:notice("Usage request for non-existing user ~s", [TargetKeyId]),
                            not_requested
                    end
            end;
        false ->
            not_requested
    end.

true_param(RD, Param) ->
    case lists:member(wrq:get_qs_value(Param, RD),
                      ["","t","true","1","y","yes"]) of
        true ->
            true;
        false ->
            case path_tokens(RD) of
                [_,Options|_] ->
                    0 < string:str(Options, Param);
                _ ->
                    false
            end
    end.

%% @doc Support both `/usage/Key/Opts/Start/End' and
%% `/usage/Key.Opts.Start.End', since some commands assume that you
%% want extra slashes quoted.
path_tokens(RD) ->
    case wrq:path_tokens(RD) of
        [JustOne] ->
            case string:chr(JustOne, $.) of
                0 ->
                    [JustOne];
                _ ->
                    %% URL is dot-separated options
                    string:tokens(JustOne, ".")
            end;
        Many ->
            Many
    end.

parse_start_time(RD) ->
    time_param(RD, "s", 3, calendar:universal_time()).

parse_end_time(RD, StartTime) ->
    time_param(RD, "e", 4, StartTime).

time_param(RD, Param, N, Default) ->
    case wrq:get_qs_value(Param, RD) of
        undefined ->
            case catch lists:nth(N, path_tokens(RD)) of
                {'EXIT', _} ->
                    {ok, Default};
                TimeString ->
                    rts:datetime(TimeString)
            end;
        TimeString ->
            rts:datetime(TimeString)
    end.

error_msg(RD, Message) ->
    {CTP, _, _} = content_types_provided(RD, #rcs_web_context{}),
    PTypes = [Type || {Type,_Fun} <- CTP],
    AcceptHdr = wrq:get_req_header("accept", RD),
    case webmachine_util:choose_media_type(PTypes, AcceptHdr) of
        ?JSON_TYPE=Type ->
            Body = json_error_msg(Message);
        _ ->
            Type = ?XML_TYPE,
            Body = xml_error_msg(Message)
    end,
    wrq:set_resp_header("content-type", Type, wrq:set_resp_body(Body, RD)).

json_error_msg(Message) ->
    MJ = [{?KEY_ERROR, [{?KEY_MESSAGE, Message}]}],
    jsx:encode(MJ).

xml_error_msg(Message) when is_binary(Message) ->
    xml_error_msg(binary_to_list(Message));
xml_error_msg(Message) ->
    Doc = [{?KEY_ERROR, [{?KEY_MESSAGE, [Message]}]}],
    riak_cs_xml:to_xml(Doc).


%% @doc Will this request require more reads than the configured limit?
-spec too_many_periods(calendar:datetime(), calendar:datetime())
          -> boolean().
too_many_periods(Start, End) ->
    Seconds = calendar:datetime_to_gregorian_seconds(End)
        - calendar:datetime_to_gregorian_seconds(Start),
    {ok, Limit} = application:get_env(riak_cs, usage_request_limit),

    {ok, Access} = riak_cs_access:archive_period(),
    {ok, Storage} = riak_cs_storage:archive_period(),

    ((Seconds div Access) > Limit) orelse ((Seconds div Storage) > Limit).


-ifdef(TEST).

datetime_test() ->
    true = proper:quickcheck(datetime_invalid_prop()).

%% make sure that datetime correctly returns 'error' for invalid
%% iso8601 date strings
datetime_invalid_prop() ->
    ?FORALL(L, list(char()),
            case rts:datetime(L) of
                {{_,_,_},{_,_,_}} ->
                    %% really, we never expect this to happen, given
                    %% that a random string is highly unlikely to be
                    %% valid iso8601, but just in case...
                    valid_iso8601(L);
                error ->
                    not valid_iso8601(L)
            end).

%% a string is considered valid iso8601 if it is of the form
%% ddddddddZddddddT, where d is a digit, Z is a 'Z' and T is a 'T'
valid_iso8601(L) ->
    length(L) == 4+2+2+1+2+2+2+1 andalso
        string:chr(L, $Z) == 4+2+2+1 andalso
        lists:all(fun is_digit/1, string:substr(L, 1, 8)) andalso
        string:chr(L, $T) == 16 andalso
        lists:all(fun is_digit/1, string:substr(L, 10, 15)).

is_digit(C) ->
    C >= $0 andalso C =< $9.

-endif. % TEST
