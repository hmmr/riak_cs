%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2015 Basho Technologies, Inc.  All Rights Reserved,
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

-module(riak_cs_yessir_riak_client).

%% Implemention status
%%
%% - [x] list buckets (only one bucket)
%% - [x] list object (only one entry)
%% - [x] get object (10-byte, fixed content)
%% - [x] put object
%% - [ ] delete object
%% - [x] GC list keys (empty key list)
%% - [ ] GC (with some keys)
%% - [x] access/storage stats PUT
%% - [ ] access/storage GET

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("riak_pb/include/riak_pb.hrl").
-include_lib("riak_pb/include/riak_kv_pb.hrl").
-include_lib("riakc/include/riakc.hrl").

-include("riak_cs.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          bucket_name,
          user,
          acl,
          manifest
         }).

init([]) ->
    {ok, fresh_state()}.

handle_call(stop, _From, State) ->
    _ = do_cleanup(State),
    {stop, normal, ok, State};
handle_call(cleanup, _From, State) ->
    {reply, ok, do_cleanup(State)};
handle_call({get_bucket, BucketName}, _From, #state{acl=Acl}=State) ->
    UserMeta = [{?MD_ACL, term_to_binary(Acl)}],
    Obj = riakc_obj:new_obj(?BUCKETS_BUCKET, BucketName, <<"vclock">>,
                            [{dict:from_list([{?MD_USERMETA, UserMeta}]),
                              BucketName}]),
    {reply, {ok, Obj}, State#state{bucket_name = BucketName}};
handle_call({set_bucket_name, BucketName}, _From, State) ->
    {reply, ok, State#state{bucket_name=BucketName}};
handle_call({get_user, UserKeyBin}, _From, State) ->
    {User, Acl} = new_user(UserKeyBin),
    Obj = user_to_robj(User),
    {reply, {ok, {Obj, true}}, State#state{user=User, acl=Acl}};
handle_call({save_user, _User, _OldUserObj}, _From, State) ->
    %% TODO: create user kicks stanchion and therefore riak, but
    %% no update the key here.
    {reply, ok, State};
handle_call({req, #rpbgetreq{} = RpbGetReq, _Timeout},
            _From, State) ->
    {Res, NewState} = process_get_req(RpbGetReq, State),
    {reply, Res, NewState};
handle_call({req, #rpbputreq{} = RpbPutReq, _Timeout},
            _From, State) ->
    {Res, NewState} = process_put_req(RpbPutReq, State),
    {reply, Res, NewState};
handle_call({req, #rpbcsbucketreq{max_results=_MaxResults,
                                  start_key=_StartKey} = _Req,
             _Timeout, {ReqId, Caller}=_Ctx}, From,
            #state{bucket_name=BucketName, acl=Acl} = State) ->
    gen_server:reply(From, {ok, ReqId}),
    RiakcObjs = manifests_to_robjs([new_manifest(BucketName, <<"yessir-key1">>, <<"1.0">>,
                                                 3141592, Acl)]),
    Caller ! {ReqId, {ok, RiakcObjs}},
    Caller ! {ReqId, {done, continuation_ignored}},
    {noreply, State};
handle_call({req, #rpblistkeysreq{bucket=?USER_BUCKET} = _Req, _Timeout,
             {ReqId, Caller}=_Ctx},
            From,
            #state{} = State) ->
    gen_server:reply(From, {ok, ReqId}),
    Keys = [list_to_binary("yessir" ++ integer_to_list(Index)) ||
               Index <- lists:seq(1, 100)],
    Caller ! {ReqId, {keys, Keys}},
    Caller ! {ReqId, done},
    {noreply, State};
handle_call({req, #rpbindexreq{bucket=?GC_BUCKET} = _Req, _Timeout},
            _From, #state{} = State) ->
    Res = {ok, #index_results_v1{keys=[], terms=[],
                                 continuation=undefined}},
    {reply, Res, State};

handle_call({set_manifest, _Manifest}, _From, State) ->
    {reply, ok, State};
handle_call({set_manifest_bag, _ManifestBagId}, _From, State) ->
    {reply, ok, State};
handle_call(master_pbc, _From, State) ->
    {reply, {ok, self()}, State};
handle_call(manifest_pbc, _From, State) ->
    {reply, {ok, self()}, State};
handle_call(block_pbc, _From, State) ->
    {reply, {ok, self()}, State};

handle_call(Request, _From, State) ->
    logger:warning("Unknown request: ~p", [Request]),
    Reply = {error, {invalid_request, Request}},
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Internal functions

process_get_req(#rpbgetreq{bucket = RiakBucket, key = VKey} = _RpbGetReq,
                #state{bucket_name = BucketName, acl = Acl} = State) ->
    case which_bucket(RiakBucket) of
        objects ->
            {Key, Vsn} = rcs_common_manifest:decompose_versioned_key(VKey),
            {M, RObj} = new_manifest_ro(BucketName, RiakBucket, Key, Vsn, Acl),
            {{ok, RObj}, State#state{manifest = M}};
        blocks ->
            UUIDSize = byte_size(VKey) - 32 div 8,
            <<_UUID:UUIDSize/binary, _BlockNumber:32>> = VKey,
            Obj = riakc_obj:new_obj(RiakBucket, VKey, <<"vclock">>,
                                    [{dict:new(), binary:copy(<<"a">>, 10)}]),
            {{ok, Obj}, State};
        Other ->
            logger:warning("Unknown #rpbgetreq{} for ~p, bucket=~p, key=~p",
                           [Other, RiakBucket, VKey]),
            error({not_implemented, {rpbgetreq, Other, RiakBucket, VKey}})
    end.

process_put_req(#rpbputreq{bucket = RiakBucket, key = VKey, return_body = 1} = _RpbPutReq,
                #state{bucket_name = BucketName, acl = Acl} = State) ->
    case which_bucket(RiakBucket) of
        objects ->
            {Key, Vsn} = rcs_common_manifest:decompose_versioned_key(VKey),
            {M, RObj} = new_manifest_ro(BucketName, RiakBucket, Key, Vsn, Acl),
            {{ok, RObj}, State#state{manifest = M}};
        Other ->
            logger:warning("Unknown #rpbgetreq{} with return_body for ~p, bucket=~p, key=~p",
                           [Other, RiakBucket, VKey]),
            error({not_implemented, {rpbgetreq_with_return_body, Other, RiakBucket, VKey}})
    end;
process_put_req(_RpbPutReq, State) ->
    {ok, State}.

new_user(UserKey) ->
    Name = <<UserKey/binary, "+Name">>,
    DisplayName = <<UserKey/binary, "+DisplayName">>,
    CanonicalId = <<UserKey/binary, "+CanonicalId">>,
    DefaultAcl = default_acl(DisplayName, CanonicalId, UserKey),
    {?RCS_USER{arn = riak_cs_aws_utils:make_user_arn(UserKey, <<"/">>),
               name = Name,
               display_name = UserKey,
               email = <<UserKey/binary, "@example.com">>,
               key_id = UserKey,
               key_secret = <<UserKey/binary, "+Secret">>,
               id = CanonicalId,
               buckets = [?RCS_BUCKET{name = <<UserKey/binary, "-yessir-bucket-1">>,
                                      acl = DefaultAcl}]},
     DefaultAcl}.

user_to_robj(?RCS_USER{arn = Arn} = User) ->
    riakc_obj:new_obj(?USER_BUCKET, Arn, <<"vclock">>,
                      [{dict:new(), term_to_binary(User)}]).

default_acl(DisplayName, CannicalId, KeyId) ->
    riak_cs_acl_utils:default_acl(DisplayName, CannicalId, KeyId).

new_manifest_ro(BucketName, RiakBucket, Key, Vsn, Acl) ->
    ContentLength = 10,
    M = new_manifest(BucketName, Key, Vsn, ContentLength, Acl),
    Dict = rcs_common_manifest_utils:new_dict(M?MANIFEST.uuid, M),
    ValueBin = riak_cs_utils:encode_term(Dict),
    RObj = riakc_obj:new_obj(RiakBucket, rcs_common_manifest:make_versioned_key(Key, Vsn), <<"vclock">>,
                            [{dict:new(), ValueBin}]),
    {M, RObj}.

new_manifest(BucketName, Key, Vsn, ContentLength, Acl) ->
    %% TODO: iteration is needed for large content length
    CMd5 = riak_cs_utils:md5(binary:copy(<<"a">>, ContentLength)),
    ?MANIFEST{block_size = riak_cs_lfs_utils:block_size(),
              bkey = {BucketName, Key},
              vsn = Vsn,
              metadata = [],
              uuid = uuid:get_v4(),

              content_length = ContentLength,
              content_type = <<"application/octet-stream">>,
              content_md5 = CMd5,
              acl = Acl,

              state = active,
              props = []}.

manifests_to_robjs(Manifests) ->
    [manifest_to_robj(M) || M <- Manifests].

manifest_to_robj(?MANIFEST{bkey={Bucket, Key}, uuid=UUID}=M) ->
    Dict = rcs_common_manifest_utils:new_dict(UUID, M),
    ManifestBucket = riak_cs_utils:to_bucket_name(objects, Bucket),
    riakc_obj:new(ManifestBucket, Key, riak_cs_utils:encode_term(Dict)).

which_bucket(?USER_BUCKET) ->
    users;
which_bucket(?BUCKETS_BUCKET) ->
    buckets;
which_bucket(?ACCESS_BUCKET) ->
    access;
which_bucket(?STORAGE_BUCKET) ->
    storage;
which_bucket(?GC_BUCKET) ->
    gc;
which_bucket(Bucket) ->
    case binary:part(Bucket, 0, 3) of
        ?OBJECT_BUCKET_PREFIX -> objects;
        ?BLOCK_BUCKET_PREFIX -> blocks
    end.

fresh_state() ->
    #state{}.

do_cleanup(_State) ->
    fresh_state().
