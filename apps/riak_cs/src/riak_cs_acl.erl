%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved,
%%               2021, 2022 TI Tokyo    All Rights Reserved.
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

%% @doc Module of functions that provide a way to determine if a
%% user may access a particular system resource.

-module(riak_cs_acl).

-include("riak_cs.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("kernel/include/logger.hrl").

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% Public API
-export([anonymous_bucket_access/3,
         anonymous_bucket_access/4,
         anonymous_object_access/4,
         anonymous_object_access/5,
         bucket_access/4,
         bucket_access/5,
         fetch_bucket_acl/2,
         bucket_acl/1,
         bucket_acl_from_contents/2,
         object_access/5,
         object_access/6,
         owner_id/2,
         exprec_detailed/1
        ]).

-define(ACL_UNDEF, {error, acl_undefined}).

%% ===================================================================
%% Public API
%% ===================================================================

%% @doc Determine if anonymous access is set for the bucket.
-spec anonymous_bucket_access(binary(), atom(), riak_client()) -> {true, string()} | false.
anonymous_bucket_access(Bucket, RequestedAccess, RcPid) ->
    anonymous_bucket_access(Bucket, RequestedAccess, RcPid, undefined).

-spec anonymous_bucket_access(binary(), atom(), riak_client(), acl()|undefined)
                             -> {true, string()} | false.
anonymous_bucket_access(_Bucket, undefined, _, _) ->
    false;
anonymous_bucket_access(Bucket, RequestedAccess, RcPid, undefined) ->
    %% Fetch the bucket's ACL
    case fetch_bucket_acl(Bucket, RcPid) of
        {ok, BucketAcl} -> anonymous_bucket_access(Bucket, RequestedAccess, RcPid, BucketAcl);
        {error, Reason} ->
            %% @TODO Think about bubbling this error up and providing
            %% feedback to requester.
            logger:error("Anonymous bucket access check failed due to error. Reason: ~p", [Reason]),
            false
    end;
anonymous_bucket_access(_Bucket, RequestedAccess, RcPid, BucketAcl) ->
    case RequestedAccess of
        'WRITE' ->
            %% Only owners may delete buckets
            false;
        _ ->
            case has_permission(acl_grants(BucketAcl), RequestedAccess) of
                true ->
                    {true, owner_id(BucketAcl, RcPid)};
                false ->
                    false
            end
    end.

%% @doc Determine if anonymous access is set for the object.
%% @TODO Enhance when doing object ACLs
-spec anonymous_object_access(riakc_obj:riakc_obj(), acl(), atom(), riak_client()) ->
                                     {true, string()} |
                                     false.
anonymous_object_access(BucketObj, ObjAcl, RequestedAccess, RcPid) ->
    anonymous_object_access(BucketObj, ObjAcl, RequestedAccess, RcPid, undefined).


-spec anonymous_object_access(riakc_obj:riakc_obj(), acl(), atom(), riak_client(), acl()|undefined) ->
                                     {true, string()} | false.
anonymous_object_access(_BucketObj, _ObjAcl, undefined, _, _) ->
    false;
anonymous_object_access(BucketObj, ObjAcl, 'WRITE', RcPid, undefined) ->
    case bucket_acl(BucketObj) of
        {ok, BucketAcl} ->
            %% `WRITE' is the only pertinent bucket-level
            %% permission when checking object access.
            anonymous_object_access(BucketObj, ObjAcl, 'WRITE', RcPid, BucketAcl);
        {error, Reason} ->
            %% @TODO Think about bubbling this error up and providing
            %% feedback to requester.
            logger:error("Anonymous object access check failed due to error. Reason: ~p", [Reason]),
            false
    end;
anonymous_object_access(_BucketObj, _ObjAcl, 'WRITE', RcPid, BucketAcl) ->
    HasObjPerm = has_permission(acl_grants(BucketAcl), 'WRITE'),
    case HasObjPerm of
        true ->
            {true, owner_id(BucketAcl, RcPid)};
        _ ->
            false
    end;
anonymous_object_access(_BucketObj, ObjAcl, RequestedAccess, RcPid, _) ->
    HasObjPerm = has_permission(acl_grants(ObjAcl), RequestedAccess),
    case HasObjPerm of
        true ->
            {true, owner_id(ObjAcl, RcPid)};
        _ ->
            false
    end.

%% @doc Determine if a user has the requested access to a bucket.
-spec bucket_access(binary(), atom(), string(), riak_client()) ->
                           boolean() | {true, string()}.
bucket_access(Bucket, RequestedAccess, CanonicalId, RcPid) ->
    bucket_access(Bucket, RequestedAccess, CanonicalId, RcPid, undefined).

-spec bucket_access(binary(), atom(), string(), riak_client(), acl()|undefined ) ->
                           boolean() | {true, string()}.
bucket_access(_Bucket, undefined, _CanonicalId, _, _) ->
    false;
bucket_access(Bucket, RequestedAccess, CanonicalId, RcPid, undefined) ->
    %% Fetch the bucket's ACL
    case fetch_bucket_acl(Bucket, RcPid) of
        {ok, Acl} ->
            bucket_access(Bucket, RequestedAccess, CanonicalId, RcPid, Acl);
        {error, notfound} ->
            %% This indicates the bucket does not exist so
            %% allow the request to proceed.
            true;
        {error, Reason} ->
            %% @TODO Think about bubbling this error up and providing
            %% feedback to requester.
            logger:error("Bucket access check failed due to error. Reason: ~p", [Reason]),
            false
    end;
bucket_access(_, RequestedAccess, CanonicalId, RcPid, Acl) ->
    IsOwner = is_owner(Acl, CanonicalId),
    HasPerm = has_permission(acl_grants(Acl),
                             RequestedAccess,
                             CanonicalId),
    case HasPerm of
        true when IsOwner == true ->
            true;
        true ->
            {true, owner_id(Acl, RcPid)};
        false when IsOwner == true
                   andalso RequestedAccess /= 'READ' ->
            true;
        _ ->
            false
    end.


-type acl_from_meta_result() :: {'ok', acl()} | {'error', 'acl_undefined'}.
-type bucket_acl_result() :: acl_from_meta_result() | {'error', 'multiple_bucket_owners'}.
-type bucket_acl_riak_error() :: {error, 'notfound' | term()}.
-spec fetch_bucket_acl(binary(), riak_client()) -> bucket_acl_result() | bucket_acl_riak_error().
fetch_bucket_acl(Bucket, RcPid) ->
    case riak_cs_bucket:fetch_bucket_object(Bucket, RcPid) of
        {ok, Obj} ->
            bucket_acl(Obj);
        {error, Reason} ->
            ?LOG_DEBUG("Failed to fetch ACL. Bucket ~p does not exist. Reason: ~p",
                       [Bucket, Reason]),
            {error, notfound}
    end.

%% @doc Get the ACL for a bucket
-spec bucket_acl(riakc_obj:riakc_obj()) -> bucket_acl_result().
bucket_acl(BucketObj) ->
    %% For buckets there should not be siblings, but in rare
    %% cases it may happen so check for them and attempt to
    %% resolve if possible.
    BucketName = riakc_obj:key(BucketObj),
    Contents = riakc_obj:get_contents(BucketObj),
    bucket_acl_from_contents(BucketName, Contents).

%% @doc Attempt to resolve an ACL for the bucket based on the contents.
%% We attempt resolution, but intentionally do not write back a resolved
%% value. Instead the fact that the bucket has siblings is logged, but the
%% condition should be rare so we avoid updating the value at this time.
bucket_acl_from_contents(_, [{MD, _}]) ->
    MetaVals = dict:fetch(?MD_USERMETA, MD),
    acl_from_meta(MetaVals);
bucket_acl_from_contents(Bucket, Contents) ->
    {Metas, Vals} = lists:unzip(Contents),
    UniqueVals = lists:usort(Vals),
    UserMetas = [dict:fetch(?MD_USERMETA, MD) || MD <- Metas],
    riak_cs_bucket:maybe_log_bucket_owner_error(Bucket, UniqueVals),
    resolve_bucket_metadata(UserMetas, UniqueVals).

resolve_bucket_metadata(Metas, [_Val]) ->
    ?LOG_DEBUG("ZZZZZZZZ Acls: ~p", [Metas]),
    Acls = [acl_from_meta(M) || M <- Metas],
    ?LOG_DEBUG("ZZZZZZZZ Acls: ~p", [Acls]),
    resolve_bucket_acls(Acls);
resolve_bucket_metadata(_Metas, _) ->
    {error, multiple_bucket_owners}.

resolve_bucket_acls([Acl]) ->
    Acl;
resolve_bucket_acls(Acls) ->
    lists:foldl(fun newer_acl/2, ?ACL_UNDEF, Acls).

newer_acl(Acl1, ?ACL_UNDEF) ->
    Acl1;
newer_acl({ok, Acl1}, {ok, Acl2})
  when Acl1?ACL.creation_time >= Acl2?ACL.creation_time ->
    {ok, Acl1};
newer_acl(_, Acl2) ->
    Acl2.

%% @doc Determine if a user has the requested access to an object
%% @TODO Enhance when doing object-level ACL work. This is a bit
%% patchy until object ACLs are done. The bucket owner gets full
%% control, but bucket-level ACLs only matter for writes otherwise.
-spec object_access(riakc_obj:riakc_obj(), acl(), atom(), string(), riak_client())
                   -> boolean() | {true, string()}.
object_access(BucketObj, ObjAcl, RequestedAccess, CanonicalId, RcPid) ->
    object_access(BucketObj, ObjAcl, RequestedAccess, CanonicalId, RcPid, undefined).


-spec object_access(riakc_obj:riakc_obj(), acl(), atom(), string(), riak_client(), undefined|acl())
                   -> boolean() | {true, string()}.
object_access(_BucketObj, _ObjAcl, undefined, _CanonicalId, _, _) ->
    false;
object_access(_BucketObj, _ObjAcl, _RequestedAccess, undefined, _RcPid, _) ->
    %% User record not provided, check for anonymous access
    anonymous_object_access(_BucketObj, _ObjAcl, _RequestedAccess, _RcPid);
object_access(BucketObj, ObjAcl, 'WRITE', CanonicalId, RcPid, undefined) ->
    case bucket_acl(BucketObj) of
        {ok, BucketAcl} ->
            object_access(BucketObj, ObjAcl, 'WRITE', CanonicalId, RcPid, BucketAcl);
        {error, Reason} ->
            %% @TODO Think about bubbling this error up and providing
            %% feedback to requester.
            logger:error("Object access check failed due to error. Reason: ~p", [Reason]),
            false
    end;
object_access(_BucketObj, _ObjAcl, 'WRITE', CanonicalId, RcPid, BucketAcl) ->
    ?LOG_DEBUG("ObjAcl: ~p; CanonicalId: ~p", [_ObjAcl, CanonicalId]),
    %% Fetch the bucket's ACL
    IsBucketOwner = is_owner(BucketAcl, CanonicalId),
    HasBucketPerm = has_permission(acl_grants(BucketAcl),
                                   'WRITE',
                                   CanonicalId),
    case HasBucketPerm of
        true when IsBucketOwner == true ->
            true;
        true ->
            {true, owner_id(BucketAcl, RcPid)};
        _ ->
            false
    end;
object_access(_BucketObj, ObjAcl, RequestedAccess, CanonicalId, RcPid, _) ->
    ?LOG_DEBUG("ObjAcl: ~p; CanonicalId: ~p", [ObjAcl, CanonicalId]),
    IsObjOwner = is_owner(ObjAcl, CanonicalId),
    HasObjPerm = has_permission(acl_grants(ObjAcl),
                                RequestedAccess,
                                CanonicalId),
    ?LOG_DEBUG("IsObjOwner: ~p", [IsObjOwner]),
    ?LOG_DEBUG("HasObjPerm: ~p", [HasObjPerm]),
    if
        (RequestedAccess == 'READ_ACP' orelse
         RequestedAccess == 'WRITE_ACP') andalso
        IsObjOwner == true ->
            %% The owner of an object may always read and modify the
            %% ACL of an object
            true;
        IsObjOwner == true andalso
        HasObjPerm == true ->
            true;
        HasObjPerm == true ->
            {true, owner_id(ObjAcl, RcPid)};
        true ->
            false
    end.

%% @doc Get the canonical id of the owner of an entity.
-spec owner_id(acl(), riak_client()) -> string().
owner_id(?ACL{owner = #{key_id := OwnerKeyId}}, _) ->
    OwnerKeyId;
owner_id(#acl_v1{owner = OwnerData}, RcPid) ->
    {Name, CanonicalId} = OwnerData,
    case riak_cs_user:get_user_by_index(?ID_INDEX,
                                        list_to_binary(CanonicalId),
                                        RcPid) of
        {ok, {Owner, _}} ->
            Owner?RCS_USER.key_id;
        {error, _} ->
            logger:warning("Failed to retrieve key_id for user ~p with canonical_id ~p", [Name, CanonicalId]),
            []
    end.

exprec_detailed(Map) ->
    Acl0 = ?ACL{grants = GG0} = exprec:frommap_acl_v3(Map),
    GG = [exprec_grant(G) || G <- GG0],
    Acl0?ACL{grants = GG}.
exprec_grant(Map) ->
    G0 = ?ACL_GRANT{perms = Perms0,
                    grantee = Grantee0} = exprec:frommap_acl_grant_v2(Map),
    G0?ACL_GRANT{perms = [binary_to_existing_atom(P, latin1) || P <- Perms0],
                 grantee = case Grantee0 of
                               #{} ->
                                   Grantee0;
                               GroupGrantee ->
                                   binary_to_existing_atom(GroupGrantee, latin1)
                           end
                }.

%% ===================================================================
%% Internal functions
%% ===================================================================

%% @doc Find the ACL in a list of metadata values and
%% convert it to an erlang term representation. Return
%% `undefined' if an ACL is not found.
acl_from_meta([]) ->
    ?ACL_UNDEF;
acl_from_meta([{?MD_ACL, Acl} | _]) ->
    {ok, Acl};
acl_from_meta([_ | RestMD]) ->
    acl_from_meta(RestMD).

%% @doc Get the grants from an ACL
-spec acl_grants(acl()) -> [acl_grant()].
acl_grants(?ACL{grants=Grants}) ->
    Grants;
acl_grants(#acl_v1{grants=Grants}) ->
    Grants.

%% @doc Iterate through a list of ACL grants and return
%% any group grants.
-spec group_grants([acl_grant()], [acl_grant()]) -> [acl_grant()].
group_grants([], GroupGrants) ->
    GroupGrants;
group_grants([HeadGrant={Grantee, _} | RestGrants],
             GroupGrants) when is_atom(Grantee) ->
    group_grants(RestGrants, [HeadGrant | GroupGrants]);
group_grants([_ | RestGrants], _GroupGrants) ->
    group_grants(RestGrants, _GroupGrants).

%% @doc Determine if the ACL grants group access
%% for the requestsed permission type.
-spec has_group_permission([{group_grant() | {term(), term()}, term()}], atom()) -> boolean().
has_group_permission([], _RequestedAccess) ->
    false;
has_group_permission([{_, Perms} | RestGrants], RequestedAccess) ->
    case check_permission(RequestedAccess, Perms) of
        true ->
            true;
        false ->
            has_group_permission(RestGrants, RequestedAccess)
    end.

%% @doc Determine if the ACL grants anonymous access
%% for the requestsed permission type.
-spec has_permission([acl_grant()], atom()) -> boolean().
has_permission(Grants, RequestedAccess) ->
    GroupGrants = group_grants(Grants, []),
    case [Perms || {Grantee, Perms} <- GroupGrants,
                   Grantee =:= 'AllUsers'] of
        [] ->
            false;
        [Perms | _] ->
            check_permission(RequestedAccess, Perms)
    end.

%% @doc Determine if a user has the requested permission
%% granted in an ACL.
-spec has_permission([acl_grant()], atom(), string()) -> boolean().
has_permission(Grants, RequestedAccess, CanonicalId) ->
    GroupGrants = group_grants(Grants, []),
    case user_grant(Grants, CanonicalId) of
        undefined ->
            has_group_permission(GroupGrants, RequestedAccess);
        {_, Perms} ->
            check_permission(RequestedAccess, Perms) orelse
                has_group_permission(GroupGrants, RequestedAccess)
    end.

%% @doc Determine if a user is the owner of a system entity.
-spec is_owner(acl(), string()) -> boolean().
is_owner(?ACL{owner = #{canonical_id := CanonicalId}}, CanonicalId) ->
    true;
is_owner(?ACL{}, _) ->
    false;
is_owner(#acl_v1{owner={_, CanonicalId}}, CanonicalId) ->
    true;
%% is_owner(#acl_v2{owner={_, CanonicalId, _}}, CanonicalId) ->
%%     true;
is_owner(#acl_v1{}, _) ->
    false.

%% @doc Check if a list of ACL permissions contains a specific permission
%% or the `FULL_CONTROL' permission.
-spec check_permission(acl_perm(), acl_perms()) -> boolean().
check_permission(_, []) ->
    false;
check_permission(Permission, [Permission | _]) ->
    true;
check_permission(_, ['FULL_CONTROL' | _]) ->
    true;
check_permission(_Permission, [_ | RestPerms]) ->
    check_permission(_Permission, RestPerms).

%% @doc Iterate through a list of ACL grants and determine
%% if there is an entry for the specified user's id. Ignore
%% any group grants.
-spec user_grant([acl_grant()], string()) -> undefined | acl_grant().
user_grant([], _) ->
    undefined;
user_grant([{Grantee, _} | RestGrants], _CanonicalId) when is_atom(Grantee) ->
    user_grant(RestGrants, _CanonicalId);
user_grant([HeadGrant={{_, CanonicalId}, _} | _], CanonicalId) ->
    HeadGrant;
user_grant([_ | RestGrants], _CanonicalId) ->
    user_grant(RestGrants, _CanonicalId).
