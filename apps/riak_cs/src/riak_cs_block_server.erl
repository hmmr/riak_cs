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

-module(riak_cs_block_server).

-behaviour(gen_server).

-ifdef(PULSE).
-include_lib("pulse/include/pulse.hrl").
-compile({parse_transform, pulse_instrument}).
-compile({pulse_replace_module,[{gen_server,pulse_gen_server}]}).
-endif.

-include("riak_cs.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("riakc/include/riakc.hrl").

%% API
-export([start_link/1, start_link/2,
         start_block_servers/3,
         get_block/5, get_block/6,
         put_block/6,
         delete_block/5,
         maybe_stop_block_servers/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         format_status/2]).

-record(state, {riak_client :: undefined | riak_client(),
                close_riak_connection=true :: boolean(),
                bag_id :: bag_id()}).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Starts the server
-spec start_link(lfs_manifest()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Manifest) ->
    gen_server:start_link(?MODULE, [Manifest], []).

-spec start_link(lfs_manifest(), riak_client()) -> {ok, pid()} | ignore | {error, term()}.
start_link(Manifest, RcPid) ->
    gen_server:start_link(?MODULE, [Manifest, RcPid], []).

%% @doc Start (up to) 'MaxNumServers'
%% riak_cs_block_server procs.
%% 'RcPid' must be a Pid you already
%% have for a riakc_pb_socket proc. If the
%% poolboy boy returns full, you will be given
%% a list of less than 'MaxNumServers'.

%% TODO: this doesn't guarantee any minimum
%% number of workers. I could also imagine
%% this function looking something
%% like:
%% start_block_servers(RcPid, MinWorkers, MaxWorkers, MinWorkerTimeout)
%% Where the function works something like:
%% Give me between MinWorkers and MaxWorkers,
%% waiting up to MinWorkerTimeout to get at least
%% MinWorkers. If the timeout occurs, this function
%% could return an error, or the pids it has
%% so far (which might be less than MinWorkers).
-spec start_block_servers(lfs_manifest(), riak_client(), pos_integer()) -> [pid()].
start_block_servers(Manifest, RcPid, MaxNumServers) ->
    start_block_servers(Manifest, RcPid, MaxNumServers, []).

start_block_servers(Manifest, RcPid, 1, BlockServers) ->
    {ok, BlockServer} = start_link(Manifest, RcPid),
    [BlockServer | BlockServers];
start_block_servers(Manifest, RcPid, NumWorkers, BlockServers) ->
    case start_link(Manifest) of
        {ok, BlockServer} ->
            start_block_servers(Manifest, RcPid, NumWorkers - 1, [BlockServer | BlockServers]);
        {error, normal} ->
            start_block_servers(Manifest, RcPid, 1, BlockServers)
    end.

-spec get_block(pid(), binary(), binary(), binary(), pos_integer()) -> ok.
get_block(Pid, Bucket, Key, UUID, BlockNumber) ->
    gen_server:cast(Pid, {get_block, self(), Bucket, Key, undefined, UUID, BlockNumber}).

%% @doc get a block which is know to have originated on cluster ClusterID.
%% If it's not found locally, it might get returned from the replication
%% cluster if a connection exists to that cluster. This is proxy-get().
-spec get_block(pid(), binary(), binary(), binary(), binary(), pos_integer()) -> ok.
get_block(Pid, Bucket, Key, ClusterID, UUID, BlockNumber) ->
    gen_server:cast(Pid, {get_block, self(), Bucket, Key, ClusterID, UUID, BlockNumber}).

-spec put_block(pid(), binary(), binary(), binary(), pos_integer(), binary()) -> ok.
put_block(Pid, Bucket, Key, UUID, BlockNumber, Value) ->
    gen_server:cast(Pid, {put_block, self(), Bucket, Key, UUID, BlockNumber,
                          Value, riak_cs_utils:md5(Value)}).

-spec delete_block(pid(), binary(), binary(), binary(), pos_integer()) -> ok.
delete_block(Pid, Bucket, Key, UUID, BlockNumber) ->
    gen_server:cast(Pid, {delete_block, self(), Bucket, Key, UUID, BlockNumber}).

-spec maybe_stop_block_servers(undefined | [pid()]) -> ok.
maybe_stop_block_servers(undefined) ->
    ok;
maybe_stop_block_servers(BlockServerPids) ->
    _ = [stop(P) || P <- BlockServerPids],
    ok.

stop(Pid) ->
    gen_server:call(Pid, stop, infinity).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([Manifest]) ->
    case riak_cs_riak_client:checkout(request_pool) of
        {ok, RcPid} ->
            init(Manifest, RcPid, #state{close_riak_connection=true});
        {error, _Reason} ->
            {stop, normal}
    end;
init([Manifest, RcPid]) ->
    init(Manifest, RcPid, #state{close_riak_connection=false}).

init(Manifest, RcPid, State) ->
    process_flag(trap_exit, true),
    ok = riak_cs_riak_client:set_manifest(RcPid, Manifest),
    BagId = riak_cs_mb_helper:bag_id_from_manifest(Manifest),
    {ok, State#state{riak_client=RcPid, bag_id=BagId}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast({get_block, ReplyPid, Bucket, Key, ClusterID, UUID, BlockNumber},
            State=#state{riak_client=RcPid, bag_id=BagId}) ->
    get_block(ReplyPid, Bucket, Key, ClusterID, BagId, UUID, BlockNumber, RcPid),
    {noreply, State};
handle_cast({put_block, ReplyPid, Bucket, Key, UUID, BlockNumber, Value, BCSum},
            State=#state{riak_client=RcPid}) ->
    {FullBucket, FullKey} = full_bkey(Bucket, Key, UUID, BlockNumber),
    MD = make_md_usermeta([{?USERMETA_BUCKET, Bucket},
                           {?USERMETA_KEY, Key},
                           {?USERMETA_BCSUM, BCSum}]),
    FailFun = fun(Error) ->
                      logger:error("Put ~p ~p UUID ~p block ~p failed: ~p",
                                   [Bucket, Key, UUID, BlockNumber, Error])
              end,
    %% TODO: Handle put failure here.
    ok = do_put_block(FullBucket, FullKey, <<>>, Value, MD,
                      RcPid, [riakc, put_block], FailFun),
    riak_cs_put_fsm:block_written(ReplyPid, BlockNumber),
    {noreply, State};
handle_cast({delete_block, ReplyPid, Bucket, Key, UUID, BlockNumber}, State=#state{riak_client=RcPid}) ->
    {FullBucket, FullKey} = full_bkey(Bucket, Key, UUID, BlockNumber),
    Timeout = riak_cs_config:get_block_timeout(),

    %% do a get first to get the vclock (only do a head request though)
    GetOptions = [head | pr_quorum_options()],
    case riak_cs_pbc:get(block_pbc(RcPid), FullBucket, FullKey,
                         GetOptions, Timeout, [riakc, head_block]) of
        {ok, RiakObject} ->
            ok = delete_block(RcPid, ReplyPid, RiakObject, {UUID, BlockNumber});
        {error, notfound} ->
            %% If the block isn't found, assume it's been
            %% previously deleted by another delete FSM, and
            %% move on to the next block.
            riak_cs_delete_fsm:block_deleted(ReplyPid, {ok, {UUID, BlockNumber}});
        {error, _} = Error ->
            %% Report errors in HEADs to prevent crashing block
            %% servers, as crash logs forces lager to sync log
            %% files at each line.
            Result = format_delete_result(Error, {UUID, BlockNumber}),
            riak_cs_delete_fsm:block_deleted(ReplyPid, Result)
    end,
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

get_block(ReplyPid, Bucket, Key, ClusterId, BagId, UUID, BlockNumber, RcPid) ->
    %% don't use proxy get if it's a local get
    %% or proxy get is disabled
    ProxyActive = riak_cs_config:proxy_get_active(),
    UseProxyGet = use_proxy_get(ClusterId, BagId),

    case riak_cs_utils:n_val_1_get_requests() of
        true ->
            do_get_block(ReplyPid, Bucket, Key, ClusterId, UseProxyGet, ProxyActive,
                         UUID, BlockNumber, RcPid);
        false ->
            get_block_legacy(ReplyPid, Bucket, Key, ClusterId,
                             UseProxyGet, UUID, BlockNumber, RcPid)
    end.

do_get_block(ReplyPid, Bucket, Key, ClusterID, UseProxyGet, ProxyActive,
             UUID, BlockNumber, RcPid) ->
    MaxRetries = riak_cs_config:get_env(riak_cs, block_get_max_retries, 5),
    do_get_block(ReplyPid, Bucket, Key, ClusterID, UseProxyGet, ProxyActive,
                 UUID, BlockNumber, RcPid, MaxRetries, []).

do_get_block(ReplyPid, _Bucket, _Key, _ClusterID, _UseProxyGet, _ProxyActive,
             UUID, BlockNumber, _RcPid, _MaxRetries, {LastReason, ErrorReasons})
  when is_list(ErrorReasons) andalso is_atom(LastReason) ->
    %% Not worth retrying, 'failure' comes as LastReason
    Sorry = {error, ErrorReasons},
    logger:error("do_get_block/11 failed. Errors: ~p", [ErrorReasons]),
    ok = riak_cs_get_fsm:chunk(ReplyPid, {UUID, BlockNumber}, Sorry);

do_get_block(ReplyPid, _Bucket, _Key, _ClusterID, _UseProxyGet, _ProxyActive,
             UUID, BlockNumber, _RcPid, MaxRetries, ErrorReasons)
  when is_list(ErrorReasons) andalso length(ErrorReasons) > MaxRetries ->
    Sorry = {error, ErrorReasons},
    logger:error("do_get_block/11 failed. Errors: ~p", [ErrorReasons]),
    ok = riak_cs_get_fsm:chunk(ReplyPid, {UUID, BlockNumber}, Sorry);

do_get_block(ReplyPid, Bucket, Key, ClusterID, UseProxyGet, ProxyActive,
             UUID, BlockNumber, RcPid, MaxRetries, ErrorReasons) ->
    ok = sleep_retries(length(ErrorReasons)),

    {FullBucket, FullKey} = full_bkey(Bucket, Key, UUID, BlockNumber),

    GetOptions1 = n_val_one_options(),
    GetOptions2 = r_one_options(),

    ProceedFun = fun(OkReply) ->
            ok = riak_cs_get_fsm:chunk(ReplyPid, {UUID, BlockNumber}, OkReply)
      end,
    RetryFun = fun(NewPause) ->
                       do_get_block(ReplyPid, Bucket, Key, ClusterID, UseProxyGet,
                                    ProxyActive, UUID, BlockNumber, RcPid, MaxRetries, NewPause)
               end,
    Timeout = riak_cs_config:local_get_block_timeout(),
    try_local_get(RcPid, FullBucket, FullKey, GetOptions1, GetOptions2,
                  Timeout, ProceedFun, RetryFun, ErrorReasons, UseProxyGet,
                  ProxyActive, ClusterID).

try_local_get(RcPid, FullBucket, FullKey, GetOptions1, GetOptions2,
              Timeout, ProceedFun, RetryFun, ErrorReasons, UseProxyGet,
              ProxyActive, ClusterID) ->
    case get_block_local(RcPid, FullBucket, FullKey, GetOptions1, Timeout,
                         [riakc, get_block_n_one]) of
        {ok, _} = Success ->
            ProceedFun(Success);
        {error, {insufficient_vnodes,_,need,_} = Reason} ->
            RetryFun([{local_one, Reason}|ErrorReasons]);
        {error, Why} when Why == notfound;
                          Why == timeout;
                          Why == disconnected;
                          Why == <<"{insufficient_vnodes,0,need,1}">>;
                          Why == {insufficient_vnodes,0,need,1} ->
            _ = riak_cs_pbc:pause_to_reconnect(block_pbc(RcPid), Why, Timeout),
            handle_local_notfound(RcPid, FullBucket, FullKey, GetOptions2,
                                  ProceedFun, RetryFun,
                                  [{local_one, Why}|ErrorReasons], UseProxyGet,
                                  ProxyActive, ClusterID);
        {error, Other} ->
            logger:error("do_get_block: other error 1: ~p", [Other]),
            RetryFun({failure, [{local_one, Other}|ErrorReasons]})
    end.


%% having spent three days trying to deal with this warning in a
%% non-violent manner:
-dialyzer([{no_match, handle_local_notfound/10}]).

handle_local_notfound(RcPid, FullBucket, FullKey, GetOptions2,
                      ProceedFun, RetryFun, ErrorReasons, UseProxyGet,
                      ProxyActive, ClusterID) ->

    Timeout = riak_cs_config:get_block_timeout(),
    case get_block_local(RcPid, FullBucket, FullKey, GetOptions2, Timeout,
                         [riakc, get_block_n_all]) of
        {ok, _} = Success ->
            ProceedFun(Success);

        {error, Why} when Why == disconnected;
                          Why == timeout ->
            logger:warning("get_block_local() failed: ~p", [Why]),
            _ = riak_cs_pbc:pause_to_reconnect(block_pbc(RcPid), Why, Timeout),
            RetryFun([{local_quorum, Why}|ErrorReasons]);

        {error, notfound} when UseProxyGet andalso ProxyActive->
            case get_block_remote(RcPid, FullBucket, FullKey, ClusterID, GetOptions2,
                                  [riakc, get_block_remote]) of
                {ok, _} = Success ->
                    ProceedFun(Success);
                {error, Reason} ->
                    RetryFun([{remote_quorum, Reason}, {local_quorum, notfound}|ErrorReasons])
            end;

        {error, notfound} when UseProxyGet ->
            RetryFun([{local_quorum, notfound}|ErrorReasons]);

        {error, notfound} ->
            RetryFun({failure, [{local_quorum, notfound}|ErrorReasons]});
        {error, Other} ->
            logger:error("do_get_block: other error 2: ~p", [Other]),
            RetryFun({failure, [{local_quorum, Other}|ErrorReasons]})
    end.

get_block_local(RcPid, FullBucket, FullKey, GetOptions, Timeout, StatsKey) ->
    case riak_cs_pbc:get(block_pbc(RcPid), FullBucket, FullKey,
                         GetOptions, Timeout, StatsKey) of
        {ok, RiakObject} ->
            resolve_block_object(RiakObject, RcPid);
        Else ->
            Else
    end.

-dialyzer([{no_match, get_block_remote/6}]).

get_block_remote(RcPid, FullBucket, FullKey, ClusterID, GetOptions0, StatsKey) ->
    %% replace get_block_timeout with proxy_get_block_timeout
    GetOptions = proplists:delete(timeout, GetOptions0),
    Timeout = riak_cs_config:proxy_get_block_timeout(),
    case riak_cs_pbc:repl_get(block_pbc(RcPid), FullBucket, FullKey,
                              ClusterID, GetOptions, Timeout, StatsKey) of
        {ok, RiakObject} ->
            resolve_block_object(RiakObject, RcPid);
        Else ->
            Else
    end.

%% @doc This is the 'legacy' block get, before we introduced the ability
%% to modify n-val per GET request.
get_block_legacy(ReplyPid, Bucket, Key, ClusterID, UseProxyGet, UUID,
                 BlockNumber, RcPid) ->
    {FullBucket, FullKey} = full_bkey(Bucket, Key, UUID, BlockNumber),
    GetOptions = r_one_options(),
    ChunkValue =
        case UseProxyGet of
            false ->
                LocalTimeout = riak_cs_config:get_block_timeout(),
                StatsKey = [riakc, get_block_legacy],
                get_block_local(block_pbc(RcPid), FullBucket, FullKey, GetOptions,
                                LocalTimeout, StatsKey);
            true ->
                get_block_remote(RcPid, FullBucket, FullKey, ClusterID, GetOptions,
                                 [riakc, get_block_legacy_remote])
        end,
    ok = riak_cs_get_fsm:chunk(ReplyPid, {UUID, BlockNumber}, ChunkValue).

delete_block(RcPid, ReplyPid, RiakObject, BlockId) ->
    Result = constrained_delete(RcPid, RiakObject, BlockId),
    _ = secondary_delete_check(Result, RcPid, RiakObject),
    riak_cs_delete_fsm:block_deleted(ReplyPid, Result),
    ok.

constrained_delete(RcPid, RiakObject, BlockId) ->
    DeleteOptions = [{r, all}, {pr, all}, {w, all}, {pw, all}],
    StatsKey = [riakc, delete_block_constrained],
    Timeout = riak_cs_config:delete_block_timeout(),
    format_delete_result(
      riak_cs_pbc:delete_obj(block_pbc(RcPid), RiakObject, DeleteOptions,
                             Timeout, StatsKey),
      BlockId).

secondary_delete_check({error, {unsatisfied_constraint, _, _}}, RcPid, RiakObject) ->
    Timeout = riak_cs_config:delete_block_timeout(),
    StatsKey = [riakc, delete_block_secondary],
    riak_cs_pbc:delete_obj(block_pbc(RcPid), RiakObject, [], Timeout, StatsKey);
secondary_delete_check({error, Reason} = E, _, _) ->
    logger:warning("Constrained block deletion failed. Reason: ~p", [Reason]),
    E;
secondary_delete_check(_, _, _) ->
    ok.

format_delete_result(ok, BlockId) ->
    {ok, BlockId};
format_delete_result({error, Reason}, BlockId) when is_binary(Reason) ->
    %% Riak client sends back oddly formatted errors
    format_delete_result({error, binary_to_list(Reason)}, BlockId);
format_delete_result({error, "{r_val_unsatisfied," ++ _}, BlockId) ->
    {error, {unsatisfied_constraint, r, BlockId}};
format_delete_result({error, "{w_val_unsatisfied," ++ _}, BlockId) ->
    {error, {unsatisfied_constraint, w, BlockId}};
format_delete_result({error, "{pr_val_unsatisfied," ++ _}, BlockId) ->
    {error, {unsatisfied_constraint, pr, BlockId}};
format_delete_result({error, "{pw_val_unsatisfied," ++ _}, BlockId) ->
    {error, {unsatisfied_constraint, pw, BlockId}};
format_delete_result(Result, _) ->
    Result.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{close_riak_connection=false}) ->
    ok;
terminate(_Reason, #state{riak_client=RcPid,
                          close_riak_connection=true}) ->

    ok = riak_cs_riak_client:checkin(request_pool, RcPid).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Define custom `format_status' to include status information of
%% internal `riak_client'.
format_status(_Opt, [_PDict, #state{riak_client=RcPid} = State]) ->
    RcState = (catch sys:get_status(RcPid)),
    [{block_server_state, State}, {riak_client_state, RcState}].


%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec full_bkey(binary(), binary(), binary(), pos_integer()) -> {binary(), binary()}.
%% @private
full_bkey(Bucket, Key, UUID, BlockId) ->
    PrefixedBucket = riak_cs_utils:to_bucket_name(blocks, Bucket),
    FullKey = riak_cs_lfs_utils:block_name(Key, UUID, BlockId),
    {PrefixedBucket, FullKey}.

find_md_usermeta(MD) ->
    dict:find(?MD_USERMETA, MD).

resolve_block_object(RObj, RcPid) ->
    {{MD, Value}, NeedRepair} =
        riak_cs_utils:resolve_robj_siblings(riakc_obj:get_contents(RObj)),
    if NeedRepair andalso is_binary(Value) ->
            RBucket = riakc_obj:bucket(RObj),
            RKey = riakc_obj:key(RObj),
            [MD1|_] = riakc_obj:get_metadatas(RObj),
            S3Info = case find_md_usermeta(MD1) of
                         {ok, Ps} ->
                             Ps;
                         error ->
                             []
                     end,
            logger:info("Repairing riak ~p ~p for ~p", [RBucket, RKey, S3Info]),
            Bucket = proplists:get_value(<<?USERMETA_BUCKET>>, S3Info),
            Key = proplists:get_value(<<?USERMETA_KEY>>, S3Info),
            VClock = riakc_obj:vclock(RObj),
            FailFun = fun(Error) ->
                              logger:error("Put S3 ~p ~p Riak ~p ~p failed: ~p",
                                           [Bucket, Key, RBucket, RKey, Error])
                      end,
            do_put_block(RBucket, RKey, VClock, Value, MD, RcPid,
                         [riakc, put_block_resolved], FailFun);
       NeedRepair andalso not is_binary(Value) ->
            logger:error("All checksums fail: ~P", [RObj]);
       true ->
            ok
    end,
    if is_binary(Value) ->
            {ok, Value};
        true ->
            {error, notfound}
    end.

make_md_usermeta(Props) ->
    dict:from_list([{?MD_USERMETA, Props}]).

do_put_block(FullBucket, FullKey, VClock, Value, MD, RcPid, StatsKey, FailFun) ->
    RiakObject0 = riakc_obj:new(FullBucket, FullKey, Value),
    RiakObject = riakc_obj:set_vclock(
            riakc_obj:update_metadata(RiakObject0, MD), VClock),
    Timeout = riak_cs_config:put_block_timeout(),
    case riak_cs_pbc:put(block_pbc(RcPid), RiakObject,
                         pw_one_options(), Timeout, StatsKey) of
        ok ->
            ok;
        Else ->
            _ = FailFun(Else),
            Else
    end.

-spec sleep_retries(non_neg_integer()) -> 'ok'.
sleep_retries(N) ->
    timer:sleep(num_retries_to_sleep_millis(N)).

-spec num_retries_to_sleep_millis(integer()) -> integer().
num_retries_to_sleep_millis(0) ->
    0;
num_retries_to_sleep_millis(N) ->
    500 * riak_cs_utils:pow(2, N).

n_val_one_options() ->
    [{r, 1}, {n_val, 1}, {sloppy_quorum, false}].

r_one_options() ->
    [{r, 1}, {notfound_ok, false}, {basic_quorum, false}].

pw_one_options() ->
    [{w, quorum}, {pw, 1}].

pr_quorum_options() ->
    [{r, quorum}, {pr, quorum}, {notfound_ok, false}, {basic_quorum, false}].

-spec use_proxy_get(cluster_id(), bag_id()) -> boolean().
use_proxy_get(undefined, _BagId) ->
    false;
use_proxy_get(SourceClusterId, BagId) when is_binary(SourceClusterId) ->
    LocalClusterID = riak_cs_mb_helper:cluster_id(BagId),
    LocalClusterID =/= SourceClusterId.

block_pbc(RcPid) ->
    {ok, BlockPbc} = riak_cs_riak_client:block_pbc(RcPid),
    BlockPbc.
