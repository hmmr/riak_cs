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

-module(riak_cs_list_roles_utils).

-include("riak_cs.hrl").

%% API
-export([get_role_list/1]).

-spec get_role_list(pid()) ->
    {ok, list_roles_response()} | {error, term()}.
get_role_list(FSMPid) ->
    gen_fsm:sync_send_all_state_event(FSMPid, get_role_list, infinity).

        

-spec filter_prefix_keys([role()], list_roles_request()) ->
          riak_cs_list_objects_utils:manifests_and_prefixes().
filter_prefix_keys(Input,
                   ?LRREQ{path_prefix = undefined}) ->
    Input;
filter_prefix_keys({Roles, CommonPrefixes},
                   ?LRREQ{path_prefix = Prefix}) ->
    PrefixFilter =
        fun(Manifest, Acc) ->
                prefix_filter(Manifest, Acc, Prefix, Delimiter)
        end,
    lists:foldl(PrefixFilter, {[], CommonPrefixes}, ManifestList).

prefix_filter(Manifest=?MANIFEST{bkey={_Bucket, Key}},
              Acc, undefined, Delimiter) ->
    Group = extract_group(Key, Delimiter),
    update_keys_and_prefixes(Acc, Manifest, <<>>, 0, Group);
prefix_filter(Manifest=?MANIFEST{bkey={_Bucket, Key}},
              {ManifestList, Prefixes}=Acc, Prefix, undefined) ->
    PrefixLen = byte_size(Prefix),
    case Key of
        << Prefix:PrefixLen/binary, _/binary >> ->
            {[Manifest | ManifestList], Prefixes};
        _ ->
            Acc
    end;
prefix_filter(Manifest=?MANIFEST{bkey={_Bucket, Key}},
              {_ManifestList, _Prefixes}=Acc, Prefix, Delimiter) ->
    PrefixLen = byte_size(Prefix),
    case Key of
        << Prefix:PrefixLen/binary, Rest/binary >> ->
            Group = extract_group(Rest, Delimiter),
            update_keys_and_prefixes(Acc, Manifest, Prefix, PrefixLen, Group);
        _ ->
            Acc
    end.

extract_group(Key, Delimiter) ->
    case binary:match(Key, [Delimiter]) of
        nomatch ->
            nomatch;
        {Pos, Len} ->
            binary:part(Key, {0, Pos+Len})
    end.

update_keys_and_prefixes({ManifestList, Prefixes},
                         Manifest, _, _, nomatch) ->
    {[Manifest | ManifestList], Prefixes};
update_keys_and_prefixes({ManifestList, Prefixes},
                         _, Prefix, PrefixLen, Group) ->
    NewPrefix = << Prefix:PrefixLen/binary, Group/binary >>,
    {ManifestList, ordsets:add_element(NewPrefix, Prefixes)}.
