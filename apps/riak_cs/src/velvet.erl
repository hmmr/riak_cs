%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved,,
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

%% @doc Client module for interacting with `stanchion' application.

-module(velvet).

-export([create_bucket/3,
         create_user/3,
         delete_user/2,
         update_user/4,
         delete_bucket/3,
         set_bucket_acl/4,
         set_bucket_policy/4,
         set_bucket_versioning/4,
         delete_bucket_policy/3,
         create_role/3,
         delete_role/2,
         update_role/4,
         create_policy/3,
         delete_policy/2,
         update_policy/4,
         create_saml_provider/3,
         delete_saml_provider/2
        ]).

-include("moss.hrl").
-include("riak_cs.hrl").
-include("aws_api.hrl").
-include_lib("kernel/include/logger.hrl").

-define(MAX_REQUEST_RETRIES, 3).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Create a bucket for a requesting party.
-spec create_bucket(string(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
create_bucket(ContentType, BucketDoc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = buckets_path(<<>>),
    Headers0 = [{"Content-Md5", content_md5(BucketDoc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('POST',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(post, Path, [201], ContentType, Headers, BucketDoc) of
        {ok, {{_, 201, _}, _RespHeaders, _RespBody}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.



%% @doc Create a bucket for a requesting party.
-spec create_user(string(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
create_user(ContentType, UserDoc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = users_path([]),
    Headers0 = [{"Content-Md5", content_md5(UserDoc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('POST',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(post, Path, [201], ContentType, Headers, UserDoc) of
        {ok, {{_, 201, _}, _RespHeaders, _RespBody}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec delete_user(binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_user(TransKeyId, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = users_path(binary_to_list(TransKeyId)),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               "",
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

%% @doc Delete a bucket. The bucket must be owned by
%% the requesting party.
-spec delete_bucket(binary(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_bucket(Bucket, Requester, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    QS = requester_qs(Requester),
    Path = buckets_path(Bucket),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               [],
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path ++ QS, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec set_bucket_acl(binary(), string(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
set_bucket_acl(Bucket, ContentType, AclDoc, Options) ->
    Path = buckets_path(Bucket, acl),
    update_bucket(Path, ContentType, AclDoc, Options, 204).

-spec set_bucket_policy(binary(), string(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
set_bucket_policy(Bucket, ContentType, PolicyDoc, Options) ->
    Path = buckets_path(Bucket, policy),
    update_bucket(Path, ContentType, PolicyDoc, Options, 204).

-spec set_bucket_versioning(binary(), string(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
set_bucket_versioning(Bucket, ContentType, Doc, Options) ->
    Path = buckets_path(Bucket, versioning),
    update_bucket(Path, ContentType, Doc, Options, 204).

%% @doc Delete a bucket. The bucket must be owned by
%% the requesting party.
-spec delete_bucket_policy(binary(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_bucket_policy(Bucket, Requester, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    QS = requester_qs(Requester),
    Path = buckets_path(Bucket, policy),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               [],
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path ++ QS, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

%% @doc Update a user record
-spec update_user(string(), binary(), binary(), proplists:proplist()) ->
          {ok, rcs_user()} | {error, reportable_error_reason()}.
update_user(ContentType, KeyId, UserDoc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = users_path(KeyId),
    Headers0 = [{"Content-Md5", content_md5(UserDoc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('PUT',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(put, Path, [200, 204], ContentType, Headers, UserDoc) of
        {ok, {_, _RespHeaders, RespBody}} ->
            User = riak_cs_iam:exprec_user(jsx:decode(list_to_binary(RespBody), [{labels, atom}])),
            {ok, User};
        {error, Error} ->
            {error, Error}
    end.


%% ===================================================================
%% Internal functions
%% ===================================================================

% @doc send request to stanchion server
% @TODO merge with create_bucket, create_user, delete_bucket
-spec update_bucket(string(), string(), binary(), proplists:proplist(), pos_integer()) ->
          ok | {error, reportable_error_reason()}.
update_bucket(Path, ContentType, Doc, Options, Expect) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('PUT',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(put, Path, [Expect], ContentType, Headers, Doc) of
        {ok, {{_, Expect, _}, _RespHeaders, _RespBody}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


%% @doc Assemble the path for a bucket request
buckets_path(Bucket) ->
    stringy(["/buckets",
             ["/" ++ binary_to_list(Bucket) || Bucket /= <<>>]]).

%% @doc Assemble the path for a bucket request
buckets_path(Bucket, acl) ->
    stringy([buckets_path(Bucket), "/acl"]);
buckets_path(Bucket, policy) ->
    stringy([buckets_path(Bucket), "/policy"]);
buckets_path(Bucket, versioning) ->
    stringy([buckets_path(Bucket), "/versioning"]).



-spec create_role(string(), binary(), proplists:proplist()) ->
          {ok, role()} | {error, reportable_error_reason()}.
create_role(ContentType, Doc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = roles_path([]),
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('POST',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(post, Path, [201], ContentType, Headers, Doc) of
        {ok, {{_, 201, _}, _RespHeaders, RespBody}} ->
            Role_ = ?IAM_ROLE{assume_role_policy_document = A} =
                riak_cs_iam:exprec_role(jsx:decode(list_to_binary(RespBody), [{labels, atom}])),
            Role = Role_?IAM_ROLE{assume_role_policy_document = base64:decode(A)},
            {ok, Role};
        {error, Error} ->
            {error, Error}
    end.

-spec delete_role(binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_role(Arn, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = roles_path(binary_to_list(Arn)),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               "",
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, {ok, {{_, StatusCode, Reason}, _RespHeaders, RespBody}}} ->
            {error, {error_status, StatusCode, Reason, RespBody}};
        {error, Error} ->
            {error, Error}
    end.

-spec update_role(string(), binary(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
update_role(ContentType, Arn, Doc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = roles_path(Arn),
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('PUT',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(put, Path, [204], ContentType, Headers, Doc) of
        {ok, {{_, 204, _}, _RespHeaders, _RespBody}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


-spec create_policy(string(), binary(), proplists:proplist()) ->
          {ok, iam_policy()} | {error, reportable_error_reason()}.
create_policy(ContentType, Doc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = policies_path([]),
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('POST',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(post, Path, [201], ContentType, Headers, Doc) of
        {ok, {{_, 201, _}, _RespHeaders, RespBody}} ->
            Policy = riak_cs_iam:exprec_iam_policy(jsx:decode(list_to_binary(RespBody), [{labels, atom}])),
            {ok, Policy};
        {error, Error} ->
            {error, Error}
    end.

-spec delete_policy(binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_policy(Arn, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = policies_path(binary_to_list(Arn)),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               "",
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.

-spec update_policy(string(), binary(), binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
update_policy(ContentType, Arn, Doc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = policies_path(Arn),
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('PUT',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(put, Path, [204], ContentType, Headers, Doc) of
        {ok, {{_, 204, _}, _RespHeaders, _RespBody}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.


-spec create_saml_provider(string(), binary(), proplists:proplist()) ->
          {ok, {binary(), [tag()]}} | {error, reportable_error_reason()}.
create_saml_provider(ContentType, Doc, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = "/samlproviders",
    Headers0 = [{"Content-Md5", content_md5(Doc)},
                {"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('POST',
                                               ContentType,
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(post, Path, [201], ContentType, Headers, Doc) of
        {ok, {{_, 201, _}, _RespHeaders, RespBody}} ->
            #{arn := Arn, tags := Tags_} = jason:decode(RespBody, [{mode, map}, {binary, v}]),
            {ok, {Arn, [#tag{key = K, value = V} || #{key := K, value := V} <- Tags_]}};
        {error, Error} ->
            {error, Error}
    end.

-spec delete_saml_provider(binary(), proplists:proplist()) ->
          ok | {error, reportable_error_reason()}.
delete_saml_provider(Arn, Options) ->
    AuthCreds = proplists:get_value(auth_creds, Options, no_auth_creds),
    Path = saml_provider_path(binary_to_list(Arn)),
    Headers0 = [{"Date", httpd_util:rfc1123_date()}],
    case AuthCreds of
        {_, _} ->
            Headers =
                [{"Authorization", auth_header('DELETE',
                                               "",
                                               Headers0,
                                               Path,
                                               AuthCreds)} |
                 Headers0];
        no_auth_creds ->
            Headers = Headers0
    end,
    case request(delete, Path, [204], Headers) of
        {ok, {{_, 204, _}, _RespHeaders, _}} ->
            ok;
        {error, Error} ->
            {error, Error}
    end.



%% -------------------------------------
%% supporting functions
%% -------------------------------------

%% @doc send an HTTP request where `Expect' is a list
%% of expected HTTP status codes.
request(Method, Url, Expect, Headers) ->
    request(Method, Url, Expect, [], Headers, []).

%% @doc send an HTTP request where `Expect' is a list
%% of expected HTTP status codes.
request(Method, Path, Expect, ContentType, Headers, Body) ->
    request(Method, Path, Expect, ContentType, Headers, Body, ?MAX_REQUEST_RETRIES).

request(Method, Path, _Expect, _ContentType, _Headers, _Body, 0) ->
    {Ip, Port, Ssl} = riak_cs_utils:stanchion_data(),
    logger:warning("Giving up trying to send a ~s request to stanchion (~s)",
                   [Method, url(Ip, Port, Ssl, Path)]),
    {error, stanchion_recovery_failure};
request(Method, Path, Expect, ContentType, Headers, Body, Attempt) ->
    stanchion_migration:validate_stanchion(),
    {Ip, Port, Ssl} = riak_cs_utils:stanchion_data(),
    Url = url(Ip, Port, Ssl, Path),

    case Method == put orelse
        Method == post of
        true ->
            Request = {Url, Headers, ContentType, Body};
        false ->
            Request = {Url, Headers}
    end,
    case httpc:request(Method, Request, [], []) of
        Resp = {ok, {{_, Status, _}, _RespHeaders, RespBody}} ->
            case lists:member(Status, Expect) of
                true ->
                    Resp;
                false ->
                    case catch jsx:decode(list_to_binary(RespBody), [{labels, atom}]) of
                        #{error_tag := Tag_,
                          resource := _Resource} ->
                            Tag = binary_to_term(base64:decode(Tag_)),
                            ?LOG_DEBUG("stanchion op non-success response, tag: ~p, resource: ~s", [Tag, _Resource]),
                            {error, Tag};
                        {'EXIT', _} ->
                            logger:error("Unexpected response from stanchion (~p). Is it up?", [Status]),
                            ?LOG_DEBUG("Stanchion response body: ~p", [RespBody]),
                            {error, stanchion_recovery_failure}
                    end
            end;
        Error ->
            if Attempt == ?MAX_REQUEST_RETRIES ->
                    %% first_call_failing_is_ok_on_startup
                    ok;
               el/=se ->
                    logger:warning("Error contacting stanchion at ~s: ~p; retrying...", [Url, Error])
            end,
            ok = stanchion_migration:adopt_stanchion(),
            request(Method, Path, Expect, ContentType, Headers, Body, Attempt - 1)
    end.


%% @doc Assemble the root URL for the given client
root_url(Ip, Port, true) ->
    ["https://", Ip, ":", integer_to_list(Port)];
root_url(Ip, Port, false) ->
    ["http://", Ip, ":", integer_to_list(Port)].

url(Ip, Port, Ssl, Path) ->
    lists:flatten(
      [root_url(Ip, Port, Ssl),
       Path
      ]).

%% @doc Calculate an MD5 hash of a request body.
content_md5(Body) ->
    base64:encode_to_string(riak_cs_utils:md5(Body)).

%% @doc Construct a MOSS authentication header
auth_header(HttpVerb, ContentType, Headers, Path, {AuthKey, AuthSecret}) ->
    Signature = velvet_auth:request_signature(HttpVerb,
                                              [{"content-type", ContentType} |
                                               Headers],
                                              Path,
                                              binary_to_list(AuthSecret)),
    lists:flatten(io_lib:format("MOSS ~s:~s", [AuthKey, Signature])).

%% @doc Assemble a requester query string for
%% user in a bucket deletion request.
requester_qs(Requester) ->
    "?requester=" ++
        mochiweb_util:quote_plus(Requester).

users_path(A) ->
    stringy(["/users", ["/" ++ mochiweb_util:quote_plus(A) || A /= []]]).

roles_path(A) ->
    stringy(["/roles", ["/" ++ mochiweb_util:quote_plus(A) || A /= []]]).

policies_path(A) ->
    stringy(["/policies", ["/" ++ mochiweb_util:quote_plus(A) || A /= []]]).

saml_provider_path(A) ->
    stringy(["/samlproviders", ["/" ++ mochiweb_util:quote_plus(A) || A /= []]]).

stringy(A) ->
    binary_to_list(iolist_to_binary(A)).
