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

%% @doc A collection functions for going to or from JSON to an erlang
%% record type.

-module(riak_cs_json).

-include("riak_cs.hrl").
-include("riak_cs_web.hrl").
-include("oos_api.hrl").
-include("aws_api.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Public API
-export([from_json/1,
         get/2,
         to_json/1,
         value_or_default/2]).

-type attributes() :: [{atom(), string()}].
-type external_node() :: {atom(), [string()]}.
-type internal_node() :: {atom(), [internal_node() | external_node()]} |
                         {atom(), attributes(), [internal_node() | external_node()]}.
-export_type([attributes/0, external_node/0, internal_node/0]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec from_json(string()) -> proplists:proplist() | {error, decode_failed}.
from_json(JsonString) ->
    case catch jsx:decode(JsonString, [{return_maps, false}]) of
        {'EXIT', _} ->
            {error, decode_failed};
        Result ->
            Result
    end.

-type match_spec() :: {index, non_neg_integer()} | {key, binary(), binary()}.
-type path_query() :: {find, match_spec()}.
-type path() :: [binary() | tuple() | path_query()].
-spec get(proplists:proplist() | undefined, path()) -> term().
get(undefined, _) ->
    {error, not_found};
get(not_found, _) ->
    {error, not_found};
get(Array, [{find, Query} | RestPath]) ->
    follow_path(find(Array, Query), RestPath);
get(Object, Path) ->
    follow_path(Object, Path).

-spec to_json(tuple() | undefined | []) -> binary().
to_json(?KEYSTONE_S3_AUTH_REQ{} = A) ->
    list_to_binary(
      jason:encode(A,
                   [{records, [{keystone_aws_auth_req_v1, record_info(fields, keystone_aws_auth_req_v1)}]}]));
to_json(?RCS_USER{} = A) ->
    list_to_binary(
      fix_quotes_for_empty_lists(
        user,
        jason:encode(A,
                     [{records, [{rcs_user_v3, record_info(fields, rcs_user_v3)},
                                 {permissions_boundary, record_info(fields, permissions_boundary)},
                                 {moss_bucket_v2, record_info(fields, moss_bucket_v2)},
                                 {acl_grant_v2, record_info(fields, acl_grant_v2)},
                                 {acl_v3, record_info(fields, acl_v3)},
                                 {tag, record_info(fields, tag)}]}])));
to_json({users, AA}) ->
    list_to_binary(
      fix_quotes_for_empty_lists(
        user,
        jason:encode(AA,
                     [{records, [{rcs_user_v3, record_info(fields, rcs_user_v3)},
                                 {permissions_boundary, record_info(fields, permissions_boundary)},
                                 {moss_bucket_v2, record_info(fields, moss_bucket_v2)},
                                 {acl_grant_v2, record_info(fields, acl_grant_v2)},
                                 {acl_v3, record_info(fields, acl_v3)},
                                 {tag, record_info(fields, tag)}]}])));
to_json(?IAM_ROLE{assume_role_policy_document = D} = A) ->
    list_to_binary(
      fix_quotes_for_empty_lists(
        role,
        jason:encode(A?IAM_ROLE{assume_role_policy_document = base64:encode(D)},
                     [{records, [{role_v1, record_info(fields, role_v1)},
                                 {role_last_used, record_info(fields, role_last_used)},
                                 {permissions_boundary, record_info(fields, permissions_boundary)},
                                 {tag, record_info(fields, tag)}]}])));
to_json(?IAM_POLICY{policy_document = D} = A) ->
    list_to_binary(
      fix_quotes_for_empty_lists(
        policy,
        jason:encode(A?IAM_POLICY{policy_document = base64:encode(D)},
                     [{records, [{iam_policy, record_info(fields, iam_policy)},
                                 {tag, record_info(fields, tag)}]}])));
to_json(?IAM_SAML_PROVIDER{saml_metadata_document = D} = A) ->
    list_to_binary(
      fix_quotes_for_empty_lists(
        saml_provider,
        jason:encode(A?IAM_SAML_PROVIDER{saml_metadata_document = base64:encode(D)},
                     [{records, [{saml_provider_v1, record_info(fields, saml_provider_v1)},
                                 {tag, record_info(fields, tag)}]}])));
to_json(?ACL{} = A) ->
    list_to_binary(
      jason:encode(A,
                   [{records, [{acl_v3, record_info(fields, acl_v3)},
                               {acl_grant_v2, record_info(fields, acl_grant_v2)}]}]));

to_json(undefined) ->
    <<>>;
to_json([]) ->
    <<>>.

fix_quotes_for_empty_lists(A, S) ->
    lists:foldl(fun({RE, Replacement}, Q) -> re:replace(Q, RE, Replacement, [global]) end, S, res(A)).
res(user) ->
    [{"\"buckets\": \"\"", "\"buckets\": []"},
     {"\"attached_policies\": \"\"", "\"attached_policies\": []"},
     {"\"tags\": \"\"", "\"tags\": []"}];
res(role) ->
    [{"\"attached_policies\": \"\"", "\"attached_policies\": []"},
     {"\"tags\": \"\"", "\"tags\": []"}];
res(policy) ->
    [{"\"statement\": \"\"", "\"statement\": []"},
     {"\"tags\": \"\"", "\"tags\": []"}];
res(saml_provider) ->
    [{"\"certificates\": \"\"", "\"certificates\": []"},
     {"\"tags\": \"\"", "\"tags\": []"}].



-spec value_or_default({ok, term()} | {error, term()}, term()) -> term().
value_or_default({error, Reason}, Default) ->
    ?LOG_DEBUG("JSON error: ~p", [Reason]),
    Default;
value_or_default({ok, Value}, _) ->
    Value.

%% ===================================================================
%% Internal functions
%% ===================================================================

follow_path(undefined, _) ->
    {error, not_found};
follow_path(Value, []) ->
    {ok, Value};
follow_path(JsonItems, [{find, Query}]) ->
    follow_path(find(JsonItems, Query), []);
follow_path(JsonItems, [{find, Query} | RestPath]) ->
    get(find(JsonItems, Query), RestPath);
follow_path(JsonItems, [Key]) when is_tuple(Key) ->
    follow_path(target_tuple_values(Key, JsonItems), []);
follow_path(JsonItems, [Key]) ->
    follow_path(proplists:get_value(Key, JsonItems), []);
follow_path(JsonItems, [Key | RestPath]) ->
    Value = proplists:get_value(Key, JsonItems),
    follow_path(Value, RestPath).

find(Array, {key, Key, Value}) ->
    lists:foldl(key_folder(Key, Value), not_found, Array);
find(Array, {index, Index}) when Index =< length(Array) ->
    lists:nth(Index, Array);
find(_, {index, _}) ->
    undefined.

key_folder(Key, Value) ->
    fun(X, Acc) ->
            case lists:keyfind(Key, 1, X) of
                {Key, Value} ->
                    X;
                _ ->
                    Acc
            end
    end.

-spec target_tuple_values(tuple(), proplists:proplist()) -> tuple().
target_tuple_values(Keys, JsonItems) ->
    list_to_tuple(
      [proplists:get_value(element(Index, Keys), JsonItems)
       || Index <- lists:seq(1, tuple_size(Keys))]).


%% ===================================================================
%% Eunit tests
%% ===================================================================
-ifdef(TEST).

get_single_key_test() ->
    Object1 = <<"{\"abc\":\"123\", \"def\":\"456\", \"ghi\":\"789\"}">>,
    Object2 = <<"{\"test\":{\"abc\":\"123\", \"def\":\"456\", \"ghi\":\"789\"}}">>,
    ?assertEqual({ok, <<"123">>}, get(from_json(Object1), [<<"abc">>])),
    ?assertEqual({ok, <<"456">>}, get(from_json(Object1), [<<"def">>])),
    ?assertEqual({ok, <<"789">>}, get(from_json(Object1), [<<"ghi">>])),
    ?assertEqual({ok, <<"123">>}, get(from_json(Object2), [<<"test">>, <<"abc">>])),
    ?assertEqual({ok, <<"456">>}, get(from_json(Object2), [<<"test">>, <<"def">>])),
    ?assertEqual({ok, <<"789">>}, get(from_json(Object2), [<<"test">>, <<"ghi">>])),
    ?assertEqual({error, not_found}, get(from_json(Object1), [<<"zzz">>])),
    ?assertEqual({error, not_found}, get(from_json(Object2), [<<"test">>, <<"zzz">>])).

get_array_test() ->
    Array = <<"[\"abc\", \"123\", \"def\", \"456\", 7]">>,
    ?assertEqual({ok, <<"abc">>}, get(from_json(Array), [{find, {index, 1}}])),
    ?assertEqual({ok, <<"123">>}, get(from_json(Array), [{find, {index, 2}}])),
    ?assertEqual({ok, <<"def">>}, get(from_json(Array), [{find, {index, 3}}])),
    ?assertEqual({ok, <<"456">>}, get(from_json(Array), [{find, {index, 4}}])),
    ?assertEqual({ok, 7}, get(from_json(Array), [{find, {index, 5}}])),
    ?assertEqual({error, not_found}, get(from_json(Array), [{find, {index, 6}}])).

get_multi_key_test() ->
    Object1 = <<"{\"test\":{\"abc\":\"123\", \"def\":\"456\", \"ghi\":\"789\"}}">>,
    Object2 = <<"{\"test\":{\"abc\":{\"123\":123,\"456\":456,\"789\":789},\"def\""
                ":{\"123\":123,\"456\":456,\"789\":789},\"ghi\":{\"123\":123,\"456\""
                ":456,\"789\":789}}}">>,
    ?assertEqual({ok, {<<"123">>, <<"789">>}}, get(from_json(Object1), [<<"test">>, {<<"abc">>, <<"ghi">>}])),
    ?assertEqual({ok, {123, 789}}, get(from_json(Object2), [<<"test">>, <<"abc">>, {<<"123">>, <<"789">>}])),
    ?assertEqual({ok, {123, 789}}, get(from_json(Object2), [<<"test">>, <<"def">>, {<<"123">>, <<"789">>}])),
    ?assertEqual({ok, {123, 789}}, get(from_json(Object2), [<<"test">>, <<"ghi">>, {<<"123">>, <<"789">>}])).

get_embedded_key_from_array_test() ->
    Object = <<"{\"test\":{\"objects\":[{\"key1\":\"a1\",\"key2\":\"a2\",\"key3\""
               ":\"a3\"},{\"key1\":\"b1\",\"key2\":\"b2\",\"key3\":\"b3\"},{\"key1\""
               ":\"c1\",\"key2\":\"c2\",\"key3\":\"c3\"}]}}">>,
    ?assertEqual({ok, [{<<"key1">>, <<"a1">>}, {<<"key2">>, <<"a2">>}, {<<"key3">>, <<"a3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key1">>, <<"a1">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"a1">>}, {<<"key2">>, <<"a2">>}, {<<"key3">>, <<"a3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key2">>, <<"a2">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"a1">>}, {<<"key2">>, <<"a2">>}, {<<"key3">>, <<"a3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key3">>, <<"a3">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"b1">>}, {<<"key2">>, <<"b2">>}, {<<"key3">>, <<"b3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key1">>, <<"b1">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"b1">>}, {<<"key2">>, <<"b2">>}, {<<"key3">>, <<"b3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key2">>, <<"b2">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"b1">>}, {<<"key2">>, <<"b2">>}, {<<"key3">>, <<"b3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key3">>, <<"b3">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"c1">>}, {<<"key2">>, <<"c2">>}, {<<"key3">>, <<"c3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key1">>, <<"c1">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"c1">>}, {<<"key2">>, <<"c2">>}, {<<"key3">>, <<"c3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key2">>, <<"c2">>}}])),
    ?assertEqual({ok, [{<<"key1">>, <<"c1">>}, {<<"key2">>, <<"c2">>}, {<<"key3">>, <<"c3">>}]},
                 get(from_json(Object),
                     [<<"test">>, <<"objects">>, {find, {key, <<"key3">>, <<"c3">>}}])).


-endif.
