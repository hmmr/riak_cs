%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2014 Basho Technologies, Inc.  All Rights Reserved,
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

-module(riak_cs_riak_client).

-behaviour(gen_server).

%% API
-export([checkout/0, checkout/1,
         checkin/1, checkin/2]).
-export([pbc_pools/0,
         pbc_pool_name/1,
         rts_puller/4]).
-export([stop/1,
         get_bucket/2,
         set_bucket_name/2,
         get_role/2,
         get_policy/2,
         get_saml_provider/2,
         get_user/2,
         get_user_with_pbc/2,
         save_user/3,
         set_manifest_bag/2,
         get_manifest_bag/1,
         set_manifest/2,
         master_pbc/1,
         manifest_pbc/1,
         block_pbc/1
        ]).
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("riak_cs.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("riakc/include/riakc.hrl").
-include_lib("kernel/include/logger.hrl").

-define(SERVER, ?MODULE).

-record(state, {
          master_pbc :: undefined | pid()
         }).

start_link(_Args) ->
    case application:get_env(riak_cs, riak_client) of
        {ok, Mod} ->
            gen_server:start_link(Mod, [], []);
        undefined ->
            Mod = case riak_cs_config:is_multibag_enabled() of
                      true  -> riak_cs_multibag_riak_client;
                      false -> ?MODULE
                  end,
            application:set_env(riak_cs, riak_client, Mod),
            gen_server:start_link(Mod, [], [])
    end.
stop(Pid) ->
    gen_server:call(Pid, stop).

-spec checkout() -> {ok, riak_client()} | {error, term()}.
checkout() ->
    checkout(request_pool).

-spec checkout(atom()) -> {ok, riak_client()} | {error, term()}.
checkout(Pool) ->
    try
        case poolboy:checkout(Pool, false) of
            full ->
                {error, all_workers_busy};
            RcPid ->
                ok = gen_server:call(RcPid, cleanup),
                {ok, RcPid}
        end
    catch
        _:Error ->
            {error, {poolboy_error, Error}}
    end.

-spec checkin(riak_client()) -> ok.
checkin(RcPid) ->
    checkin(request_pool, RcPid).

-spec checkin(atom(), riak_client()) -> ok.
checkin(Pool, RcPid) ->
    ok = gen_server:call(RcPid, cleanup),
    poolboy:checkin(Pool, RcPid).

-spec pbc_pools() -> [atom()].
pbc_pools() ->
    [pbc_pool_name(B) || {B, _, _} <- riak_cs_mb_helper:bags()].

-spec pbc_pool_name(master | bag_id()) -> atom().
pbc_pool_name(master) ->
    pbc_pool_master;
pbc_pool_name(undefined) ->
    pbc_pool_master;
pbc_pool_name(BagId) when is_binary(BagId) ->
    list_to_atom(lists:flatten(io_lib:format("pbc_pool_~s", [BagId]))).

%% @doc Make a thunk that looks up samples for a given bucket and suffix.
-spec rts_puller(riak_client(), binary(), iolist(), riak_cs_stats:key()) -> fun().
rts_puller(RcPid, Bucket, Suffix, StatsKey) ->
    fun(Slice, {Samples, Errors}) ->
            {ok, MasterPbc} = riak_cs_riak_client:master_pbc(RcPid),
            Timeout = riak_cs_config:get_access_timeout(),
            case riak_cs_pbc:get(MasterPbc, Bucket, rts:slice_key(Slice, Suffix), [],
                                 Timeout, StatsKey) of
                {ok, Object} ->
                    RawSamples =
                        [ catch element(2, {struct,_}=mochijson2:decode(V))
                          || V <- riakc_obj:get_values(Object) ],
                    {NewSamples, EncodingErrors} =
                        lists:partition(fun({'EXIT',_}) -> false;
                                           (_)          -> true
                                        end,
                                        RawSamples),
                    {NewSamples++Samples,
                     [{Slice, {encoding, length(EncodingErrors)}}
                      || EncodingErrors /= []]
                     ++Errors};
                {error, notfound} ->
                    %% this is normal - we ask for all possible
                    %% archives, and just deal with the ones that exist
                    {Samples, Errors};
                {error, Error} ->
                    {Samples, [{Slice, Error}|Errors]}
            end
    end.

-spec get_bucket(riak_client(), binary()) -> {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_bucket(RcPid, BucketName) when is_binary(BucketName) ->
    gen_server:call(RcPid, {get_bucket, BucketName}, infinity).

-spec set_bucket_name(riak_client(), binary()) -> ok | {error, term()}.
set_bucket_name(RcPid, BucketName) when is_binary(BucketName) ->
    gen_server:call(RcPid, {set_bucket_name, BucketName}, infinity).

%% @doc Perform an initial read attempt with R=PR=N.
%% If the initial read fails retry using
%% R=quorum and PR=1, but indicate that bucket deletion
%% indicators should not be cleaned up.
-spec get_user(riak_client(),
               UserKey :: binary()) ->
                      {ok, {riakc_obj:riakc_obj(), KeepDeletedBuckets :: boolean()}} |
                      {error, term()}.
get_user(RcPid, UserKey) when is_binary(UserKey) ->
    gen_server:call(RcPid, {get_user, UserKey}, infinity).

-spec save_user(riak_client(), rcs_user(), riakc_obj:riakc_obj()) -> ok | {error, term()}.
save_user(RcPid, User, OldUserObj) ->
    gen_server:call(RcPid, {save_user, User, OldUserObj}, infinity).


-spec set_manifest(riak_client(), lfs_manifest()) -> ok | {error, term()}.
set_manifest(RcPid, Manifest) ->
    gen_server:call(RcPid, {set_manifest, {Manifest?MANIFEST.uuid, Manifest}}).

-spec set_manifest_bag(riak_client(), binary()) -> ok | {error, term()}.
set_manifest_bag(RcPid, ManifestBagId) ->
    gen_server:call(RcPid, {set_manifest_bag, ManifestBagId}).

-spec get_manifest_bag(riak_client()) -> {ok, binary()} | {error, term()}.
get_manifest_bag(RcPid) ->
    gen_server:call(RcPid, get_manifest_bag).

-spec get_role(riak_client(), binary()) -> {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_role(RcPid, A) ->
    gen_server:call(RcPid, {get_role, A}, infinity).

-spec get_policy(riak_client(), binary()) -> {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_policy(RcPid, A) ->
    gen_server:call(RcPid, {get_policy, A}, infinity).

-spec get_saml_provider(riak_client(), binary()) -> {ok, riakc_obj:riakc_obj()} | {error, term()}.
get_saml_provider(RcPid, Arn) ->
    gen_server:call(RcPid, {get_saml_provider, Arn}, infinity).

%% TODO: Using this function is more or less a cheat.
%% It's better to export new  function to manipulate manifests
%% from this module.
-spec master_pbc(riak_client()) -> {ok, MasterPbc::pid()} | {error, term()}.
master_pbc(RcPid) ->
    gen_server:call(RcPid, master_pbc).

%% TODO: Also this is cheat
-spec manifest_pbc(riak_client()) -> {ok, ManifetPbc::pid()} | {error, term()}.
manifest_pbc(RcPid) ->
    gen_server:call(RcPid, manifest_pbc).

%% TODO: Also this is cheat
-spec block_pbc(riak_client()) -> {ok, BlockPbc::pid()} | {error, term()}.
block_pbc(RcPid) ->
    gen_server:call(RcPid, block_pbc).

%%% Internal functions

init([]) ->
    {ok, fresh_state()}.

handle_call(stop, _From, State) ->
    _ = do_cleanup(State),
    {stop, normal, ok, State};
handle_call(cleanup, _From, State) ->
    {reply, ok, do_cleanup(State)};
handle_call({get_bucket, BucketName}, _From, State0) ->
    case do_get_from_bucket(?BUCKETS_BUCKET, BucketName, get_cs_bucket, State0) of
        {ok, Obj, State9} ->
            {reply, {ok, Obj}, State9};
        {error, Reason, State9} ->
            {reply, {error, Reason}, State9}
    end;
handle_call({set_bucket_name, _BucketName}, _From, State) ->
    {reply, ok, State};
handle_call({get_user, UserKey}, _From, State) ->
    case ensure_master_pbc(State) of
        {ok, #state{master_pbc = MasterPbc} = NewState} ->
            Res = get_user_with_pbc(MasterPbc, UserKey),
            {reply, Res, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({save_user, User, OldUserObj}, _From, State) ->
    case ensure_master_pbc(State) of
        {ok, #state{master_pbc=MasterPbc} = NewState} ->
            Res = save_user_with_pbc(MasterPbc, User, OldUserObj),
            {reply, Res, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({get_role, Id}, _From, State0) ->
    case do_get_from_bucket(?IAM_ROLE_BUCKET, Id, get_cs_role, State0) of
        {ok, Obj, State9} ->
            {reply, {ok, Obj}, State9};
        {error, Reason, State9} ->
            {reply, {error, Reason}, State9}
    end;
handle_call({get_policy, Id}, _From, State0) ->
    case do_get_from_bucket(?IAM_POLICY_BUCKET, Id, get_cs_policy, State0) of
        {ok, Obj, State9} ->
            {reply, {ok, Obj}, State9};
        {error, Reason, State9} ->
            {reply, {error, Reason}, State9}
    end;
handle_call({get_saml_provider, Id}, _From, State0) ->
    case do_get_from_bucket(?IAM_SAMLPROVIDER_BUCKET, Id, get_cs_saml_provider, State0) of
        {ok, Obj, State9} ->
            {reply, {ok, Obj}, State9};
        {error, Reason, State9} ->
            {reply, {error, Reason}, State9}
    end;
handle_call(master_pbc, _From, State) ->
    case ensure_master_pbc(State) of
        {ok, #state{master_pbc=MasterPbc} = NewState} ->
            {reply, {ok, MasterPbc}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call(manifest_pbc, _From, State) ->
    case ensure_master_pbc(State) of
        {ok, #state{master_pbc=MasterPbc} = NewState} ->
            {reply, {ok, MasterPbc}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;
handle_call({set_manifest, {_UUID, _Manifest}}, _From, State) ->
    {reply, ok, State};
handle_call({set_manifest_bag, _ManifestBagId}, _From, State) ->
    {reply, ok, State};
handle_call(get_manifest_bag, _From, State) ->
    {reply, {ok, master}, State};
handle_call(block_pbc, _From, State) ->
    case ensure_master_pbc(State) of
        {ok, #state{master_pbc=MasterPbc} = NewState} ->
            {reply, {ok, MasterPbc}, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(Request, _From, State) ->
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

fresh_state() ->
    #state{}.

do_cleanup(State) ->
    stop_pbc(State#state.master_pbc),
    fresh_state().

stop_pbc(undefined) ->
    ok;
stop_pbc(Pbc) when is_pid(Pbc) ->
    riak_cs_utils:close_riak_connection(pbc_pool_name(master), Pbc),
    ok.

do_get_from_bucket(Bucket, Key, StatsItem, State0) ->
    case ensure_master_pbc(State0) of
        {ok, State9 = #state{master_pbc = Pbc}} ->
            case get_object_with_pbc(Pbc, Bucket, Key, StatsItem) of
                {ok, Obj} ->
                    {ok, Obj, State9};
                {error, Reason} ->
                    {error, Reason, State0}
            end;
        {error, Reason} ->
            {error, Reason, State0}
    end.

ensure_master_pbc(#state{master_pbc = MasterPbc} = State)
  when is_pid(MasterPbc) ->
    {ok, State};
ensure_master_pbc(#state{} = State) ->
    case riak_cs_utils:riak_connection(pbc_pool_name(master)) of
        {ok, MasterPbc} -> {ok, State#state{master_pbc=MasterPbc}};
        {error, Reason} -> {error, Reason}
    end.

get_object_with_pbc(MasterPbc, Bucket, Key, StatsItem) ->
    Timeout = riak_cs_config:get_bucket_timeout(),
    riak_cs_pbc:get(MasterPbc, Bucket, Key, [], Timeout,
                    [riakc, StatsItem]).

get_user_with_pbc(MasterPbc, Key) ->
    get_user_with_pbc(MasterPbc, Key, riak_cs_config:fast_user_get()).

get_user_with_pbc(MasterPbc, Key, true) ->
    weak_get_user_with_pbc(MasterPbc, Key);
get_user_with_pbc(MasterPbc, Key, false) ->
    case strong_get_user_with_pbc(MasterPbc, Key) of
        {ok, _} = OK -> OK;
        {error, <<"{pr_val_unsatisfied,", _/binary>>} ->
            weak_get_user_with_pbc(MasterPbc, Key);
        {error, Reason} ->
            logger:warning("Fetching user record with strong option failed: ~p", [Reason]),
            Timeout = riak_cs_config:get_user_timeout(),
            _ = riak_cs_pbc:pause_to_reconnect(MasterPbc, Reason, Timeout),
            weak_get_user_with_pbc(MasterPbc, Key)
    end.

strong_get_user_with_pbc(MasterPbc, Key) ->
    StrongOptions = [{r, all}, {pr, all}, {notfound_ok, false}],
    Timeout = riak_cs_config:get_user_timeout(),
    case riak_cs_pbc:get(MasterPbc, ?USER_BUCKET, Key, StrongOptions,
                         Timeout, [riakc, get_cs_user_strong]) of
        {ok, Obj} ->
            %% since we read from all primaries, we're less concerned
            %% with there being an 'out-of-date' replica that we might
            %% conflict with (and not be able to properly resolve
            %% conflicts).
            KeepDeletedBuckets = false,
            {ok, {Obj, KeepDeletedBuckets}};
        {error, _} = Error ->
            Error
    end.

weak_get_user_with_pbc(MasterPbc, Key) ->
    Timeout = riak_cs_config:get_user_timeout(),
    WeakOptions = [{r, quorum}, {pr, one}, {notfound_ok, false}],
    case riak_cs_pbc:get(MasterPbc, ?USER_BUCKET, Key, WeakOptions,
                         Timeout, [riakc, get_cs_user]) of
        {ok, Obj} ->
            %% We weren't able to read from all primary vnodes, so
            %% don't risk losing information by pruning the bucket
            %% list.
            KeepDeletedBuckets = true,
            {ok, {Obj, KeepDeletedBuckets}};
        {error, Reason} ->
            {error, Reason}
    end.

save_user_with_pbc(MasterPbc, User, OldUserObj) ->
    Indexes = [{?EMAIL_INDEX, User?RCS_USER.email},
               {?ID_INDEX, User?RCS_USER.canonical_id}],
    MD = dict:store(?MD_INDEX, Indexes, dict:new()),
    UpdUserObj = riakc_obj:update_metadata(
                   riakc_obj:update_value(OldUserObj,
                                          riak_cs_utils:encode_term(User)),
                   MD),
    Timeout = riak_cs_config:put_user_timeout(),
    riak_cs_pbc:put(MasterPbc, UpdUserObj, Timeout, [riakc, put_cs_user]).
