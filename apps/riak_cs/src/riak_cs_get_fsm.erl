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

%% @doc get fsm for Riak CS.

-module(riak_cs_get_fsm).

-behaviour(gen_fsm).

-ifdef(PULSE).
-include_lib("pulse/include/pulse.hrl").
-compile({parse_transform, pulse_instrument}).
-compile({pulse_replace_module,[{gen_fsm,pulse_gen_fsm}]}).
-endif.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

%% Test API
-export([test_link/6]).
-endif.

-include("riak_cs.hrl").
-include_lib("kernel/include/logger.hrl").

%% API
-export([start_link/7,
         stop/1,
         continue/2,
         manifest/2,
         chunk/3,
         get_manifest/1,
         get_next_chunk/1]).

%% gen_fsm callbacks
-export([init/1,
         prepare/2,
         prepare/3,
         waiting_value/2,
         waiting_value/3,
         waiting_continue_or_stop/2,
         waiting_continue_or_stop/3,
         waiting_chunks/2,
         waiting_chunks/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-type block_name() :: {binary(), integer()}.

-type block_queue() :: queue:queue().

-record(state, {from :: undefined | {pid(), reference()},
                riak_client :: riak_client(),
                mani_fsm_pid :: undefined | pid(),
                bucket :: binary(),
                caller :: reference(),
                key :: term(),
                obj_vsn :: binary(),
                fetch_concurrency :: pos_integer(),
                buffer_factor :: pos_integer(),
                got_blocks=orddict:new() :: orddict:orddict(),
                manifest :: term(),
                blocks_order = [] :: [block_name()],
                blocks_intransit = queue:new() :: block_queue(),
                test = false :: boolean(),
                total_blocks :: undefined | pos_integer(),
                num_sent=0 :: non_neg_integer(),
                initial_block :: undefined | block_name(),
                final_block :: undefined | block_name(),
                skip_bytes_initial = 0 :: non_neg_integer(),
                keep_bytes_final = 0 :: non_neg_integer(),
                free_readers :: undefined | [pid()],
                all_reader_pids :: undefined | [pid()]}).

%% ===================================================================
%% Public API
%% ===================================================================

-spec start_link(binary(), binary(), binary(), pid(), riak_client(), pos_integer(),
                 pos_integer()) -> {ok, pid()} | {error, term()}.
start_link(Bucket, Key, ObjVsn, Caller, RcPid, FetchConcurrency, BufferFactor) ->
    gen_fsm:start_link(?MODULE, [Bucket, Key, ObjVsn, Caller, RcPid,
                                 FetchConcurrency, BufferFactor], []).

stop(Pid) ->
    gen_fsm:send_event(Pid, stop).

continue(Pid, Range) ->
    gen_fsm:send_event(Pid, {continue, Range}).

get_manifest(Pid) ->
    gen_fsm:sync_send_event(Pid, get_manifest, infinity).

get_next_chunk(Pid) ->
    gen_fsm:sync_send_event(Pid, get_next_chunk, infinity).

manifest(Pid, ManifestValue) ->
    gen_fsm:send_event(Pid, {object, self(), ManifestValue}).

chunk(Pid, ChunkSeq, ChunkValue) ->
    gen_fsm:send_event(Pid, {chunk, self(), {ChunkSeq, ChunkValue}}).

%% ====================================================================
%% gen_fsm callbacks
%% ====================================================================

-ifndef(TEST).

init([Bucket, Key, ObjVsn, Caller, RcPid, FetchConcurrency, BufferFactor])
  when is_binary(Bucket), is_binary(Key), is_pid(Caller),
       is_pid(RcPid),
       FetchConcurrency > 0, BufferFactor > 0 ->

    %% We need to do this (the monitor) for two reasons
    %% 1. We're started through a supervisor, so the
    %%    proc that actually intends to start us isn't
    %%    linked to us.
    %% 2. Even if we didn't use a supervisor, the webmachine
    %%    process uses exit(..., normal), even on abnormal
    %%    terminations, so this process would still
    %%    live.
    CallerRef = erlang:monitor(process, Caller),
    %% we want to trap exits because
    %% `erlang:link` isn't atomic, and
    %% since we're starting the reader
    %% through a supervisor we can't use
    %% `spawn_link`. If the process has already
    %% died before we call link, we'll get
    %% an exit Reason of `noproc`
    process_flag(trap_exit, true),

    State = #state{bucket = Bucket,
                   key = Key,
                   obj_vsn = ObjVsn,
                   caller = CallerRef,
                   riak_client = RcPid,
                   buffer_factor = BufferFactor,
                   fetch_concurrency = FetchConcurrency},
    {ok, prepare, State, 0}.

-else.

init([Bucket, Key, ObjVsn, Caller, RcPid, FetchConcurrency, BufferFactor])
  when is_binary(Bucket), is_binary(Key), is_pid(Caller),
       is_pid(RcPid),
       FetchConcurrency > 0, BufferFactor > 0 ->

    CallerRef = erlang:monitor(process, Caller),
    process_flag(trap_exit, true),

    State = #state{bucket = Bucket,
                   key = Key,
                   obj_vsn = ObjVsn,
                   caller = CallerRef,
                   riak_client = RcPid,
                   buffer_factor = BufferFactor,
                   fetch_concurrency = FetchConcurrency},
    {ok, prepare, State, 0};

init([test, Bucket, Key, Caller, ContentLength,
      BlockSize, FetchConcurrency, BufferFactor]) ->
    {ok, prepare, State1, 0} = init([Bucket, Key, ?LFS_DEFAULT_OBJECT_VERSION, Caller, self(),
                                     FetchConcurrency, BufferFactor]),

    %% purposely have the timeout happen
    %% so that we get called in the prepare
    %% state
    RPs = [begin
               {ok, ReaderPid} =
                   riak_cs_dummy_reader:start_link([self(),
                                                    Bucket,
                                                    Key,
                                                    ?LFS_DEFAULT_OBJECT_VERSION,
                                                    ContentLength,
                                                    BlockSize]),
               link(ReaderPid),
               ReaderPid
           end || _ <- lists:seq(1, FetchConcurrency)],
    {ok, Manifest} = riak_cs_dummy_reader:get_manifest(hd(RPs)),
    {ok, waiting_value, State1#state{free_readers=RPs,
                                     manifest=Manifest,
                                     test=true}}.

-endif.

prepare(timeout, State) ->
    NewState = prepare(State),
    {next_state, waiting_value, NewState}.

prepare(get_manifest, _From, State) ->
    PreparedState = prepare(State),
    case PreparedState#state.manifest of
        undefined ->
            {stop, normal, notfound, PreparedState};
        Mfst ->
            NextStateTimeout = 60000,
            NewState = PreparedState#state{from=undefined},
            {reply, Mfst, waiting_continue_or_stop, NewState, NextStateTimeout}
    end.

waiting_value(stop, State) ->
    {stop, normal, State}.

waiting_value(get_manifest, _From, State=#state{manifest=undefined}) ->
    {stop, normal, notfound, State};
waiting_value(get_manifest, _From, State=#state{manifest=Mfst}) ->
    NextStateTimeout = 60000,
    NewState = State#state{from=undefined},
    {reply, Mfst, waiting_continue_or_stop, NewState, NextStateTimeout}.

waiting_continue_or_stop(timeout, State) ->
    {stop, normal, State};
waiting_continue_or_stop(stop, State) ->
    {stop, normal, State};
waiting_continue_or_stop({continue, Range}, #state{manifest = Manifest,
                                                   bucket = BucketName,
                                                   key = Key,
                                                   obj_vsn = ObjVsn,
                                                   fetch_concurrency = FetchConcurrency,
                                                   free_readers = Readers,
                                                   riak_client = RcPid} = State) ->
    {BlocksOrder, SkipInitial, KeepFinal} =
        riak_cs_lfs_utils:block_sequences_for_manifest(Manifest, Range),
    case BlocksOrder of
        [] ->
            %% We should never get here because empty
            %% files are handled by the wm resource.
            logger:warning("~p/~p:~p has no blocks", [BucketName, Key, ObjVsn]),
            {stop, normal, State};
        [InitialBlock|_] ->
            TotalBlocks = length(BlocksOrder),

            %% Start the block servers
            case Readers of
                undefined ->
                    FreeReaders =
                    riak_cs_block_server:start_block_servers(Manifest, RcPid,
                        FetchConcurrency),
                    ?LOG_DEBUG("Block Servers: ~p", [FreeReaders]);
                _ ->
                    FreeReaders = Readers
            end,
            %% start retrieving the first set of blocks
            UpdState = State#state{blocks_order = BlocksOrder,
                                   total_blocks = TotalBlocks,
                                   initial_block = InitialBlock,
                                   final_block = lists:last(BlocksOrder),
                                   skip_bytes_initial = SkipInitial,
                                   keep_bytes_final = KeepFinal,
                                   free_readers = FreeReaders},
            {next_state, waiting_chunks, read_blocks(UpdState)}
    end.

waiting_continue_or_stop(Event, From, State) ->
    logger:info("Pid ~p got unknown event ~p from ~p", [self(), Event, From]),
    {next_state, waiting_continue_or_stop, State}.

waiting_chunks(get_next_chunk, From, State=#state{num_sent = TotalNumBlocks,
                                                  total_blocks = TotalNumBlocks}) ->
    _ = gen_fsm:reply(From, {done, <<>>}),
    {stop, normal, State};
waiting_chunks(get_next_chunk, From, State) ->
    case perhaps_send_to_user(From, State) of
        done ->
            UpdState = State#state{from = From},
            {next_state, waiting_chunks, read_blocks(UpdState)};
        {sent, UpdState} ->
            Got = UpdState#state.got_blocks,
            GotSize = orddict:size(Got),
            MaxGotSize = riak_cs_lfs_utils:get_fsm_buffer_size_factor(),
            if GotSize < MaxGotSize ->
                {next_state, waiting_chunks, UpdState, 0};
            true ->
                {next_state, waiting_chunks, UpdState}
            end;
        {not_sent, UpdState} ->
            {next_state, waiting_chunks, read_blocks(UpdState)}
    end.

perhaps_send_to_user(From, #state{got_blocks = Got,
                                  num_sent = NumSent,
                                  blocks_intransit = Intransit} = State) ->
    case queue:out(Intransit) of
        {empty, _} ->
            done;
        {{value, NextBlock}, UpdIntransit} ->
            case orddict:find(NextBlock, Got) of
                {ok, Block} ->
                    ?LOG_DEBUG("Returning block ~p to client", [NextBlock]),
                    %% Must use gen_fsm:reply/2 here!  We are shared
                    %% with an async event func and must return next_state.
                    gen_fsm:reply(From, {chunk, Block}),
                    {sent, State#state{got_blocks = orddict:erase(NextBlock, Got),
                                       num_sent = NumSent+1,
                                       blocks_intransit = UpdIntransit}};
                error ->
                    {not_sent, State#state{from=From}}
            end
    end.

waiting_chunks(stop, State) ->
    {stop, normal, State};
waiting_chunks(timeout, State = #state{got_blocks = Got}) ->
    GotSize = orddict:size(Got),
    ?LOG_DEBUG("starting fetch again with ~p left in queue", [GotSize]),
    UpdState = read_blocks(State),
    {next_state, waiting_chunks, UpdState};

waiting_chunks({chunk, Pid, {NextBlock, BlockReturnValue}},
               #state{from = From,
                      got_blocks = Got,
                      free_readers = FreeReaders,
                      initial_block = InitialBlock,
                      final_block = FinalBlock,
                      skip_bytes_initial = SkipInitial,
                      keep_bytes_final = KeepFinal
                     } = State) ->
    ?LOG_DEBUG("Retrieved block ~p", [NextBlock]),
    case BlockReturnValue of
        {error, _} = ErrorRes ->
            #state{bucket = Bucket, key = Key, obj_vsn = ObjVsn} = State,
            logger:error("Cannot get S3 ~s/~s:~s block# ~p: ~p",
                         [Bucket, Key, ObjVsn, NextBlock, ErrorRes]),
            %% Our terminate() will explicitly stop dependent processes,
            %% we don't need an abnormal exit to kill them for us.
            exit(normal);
        {ok, _} ->
            ok
    end,
    {ok, RawBlockValue} = BlockReturnValue,        % TODO: robustify!
    BlockValue = trim_block_value(RawBlockValue,
                                  NextBlock,
                                  {InitialBlock, FinalBlock},
                                  {SkipInitial, KeepFinal}),
    UpdGot = orddict:store(NextBlock, BlockValue, Got),
    %% TODO: ?LOG_DEBUG("BlocksLeft: ~p", [BlocksLeft]),
    GotSize = orddict:size(UpdGot),
    UpdState0 = State#state{got_blocks = UpdGot, free_readers = [Pid|FreeReaders]},
    MaxGotSize = riak_cs_lfs_utils:get_fsm_buffer_size_factor(),
    UpdState = if GotSize < MaxGotSize ->
        read_blocks(UpdState0);
    true ->
        UpdState0
    end,

    if From == undefined ->
            {next_state, waiting_chunks, UpdState};
       true ->
            case perhaps_send_to_user(From, UpdState) of
                {sent, Upd2State} ->
                    {next_state, waiting_chunks, Upd2State#state{from = undefined}};
                {not_sent, Upd2State} ->
                    {next_state, waiting_chunks, Upd2State}
            end
    end.

%% @private
handle_event(_Event, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop,badmsg,StateData}.

%% @private
handle_info(request_timeout, StateName, StateData) ->
    ?MODULE:StateName(request_timeout, StateData);
%% TODO:
%% we don't want to just
%% stop whenever a reader is
%% killed once we have some concurrency
%% in our readers. But since we just
%% have one reader process now, if it dies,
%% we have no reason to stick around
%%
%% @TODO Also handle reader pid death
handle_info({'EXIT', ManiPid, _Reason}, _StateName, StateData = #state{mani_fsm_pid = ManiPid}) ->
    {stop, normal, StateData};
handle_info({'DOWN', CallerRef, process, _Pid, Reason},
            _StateName,
            State = #state{caller = CallerRef}) ->
    {stop, Reason, State};
handle_info({'EXIT', _Pid, normal}, StateName, StateData) ->
    %% TODO: who is _Pid when clean_multipart_unused_parts returns updated?
    {next_state, StateName, StateData};
handle_info(_Info, _StateName, StateData) ->
    {stop, {badmsg, _Info}, StateData}.

%% @private
terminate(_Reason, _StateName, #state{test = false,
                                      all_reader_pids = BlockServerPids,
                                      mani_fsm_pid = ManiPid}) ->
    riak_cs_manifest_fsm:maybe_stop_manifest_fsm(ManiPid),
    riak_cs_block_server:maybe_stop_block_servers(BlockServerPids),
    ok;
terminate(_Reason, _StateName, #state{test=true}) ->
    ok.

%% @private
code_change(_OldVsn, StateName, State, _Extra) -> {ok, StateName, State}.

%% ===================================================================
%% Internal functions
%% ===================================================================

prepare(#state{bucket = Bucket,
               key = Key,
               obj_vsn = Vsn,
               riak_client = RcPid} = State) ->
    %% start the process that will
    %% fetch the value, be it manifest
    %% or regular object
    {ok, ManiPid} = riak_cs_manifest_fsm:start_link(Bucket, Key, Vsn, RcPid),
    case riak_cs_manifest_fsm:get_active_manifest(ManiPid) of
        {ok, Manifest} ->
            case riak_cs_mp_utils:clean_multipart_unused_parts(Manifest, RcPid) of
                same ->
                    State#state{manifest = Manifest,
                                mani_fsm_pid = ManiPid};
                updated ->
                    riak_cs_manifest_fsm:stop(ManiPid),
                    prepare(State)
            end;
        {error, notfound} ->
            State#state{mani_fsm_pid = ManiPid}
    end.

read_blocks(#state{free_readers=[]} = State) ->
    State;
read_blocks(#state{blocks_order=[]} = State) ->
    State;
read_blocks(#state{manifest=Manifest,
                   bucket=Bucket,
                   key=Key,
                   free_readers=[ReaderPid | RestFreeReaders],
                   blocks_order=[NextBlock|BlocksOrder],
                   blocks_intransit=Intransit} = State) ->
    ClusterID = cluster_id_or_default(Manifest?MANIFEST.cluster_id),
    {UUID, Seq} = NextBlock,
    %% no need to thread ObjVsnName to block server as it forms its
    %% keys from uuids (in turn from corresponding version manifests)
    riak_cs_block_server:get_block(ReaderPid, Bucket, Key, ClusterID, UUID, Seq),
    read_blocks(State#state{free_readers=RestFreeReaders,
                            blocks_order=BlocksOrder,
                            blocks_intransit=queue:in(NextBlock, Intransit)}).

-spec cluster_id_or_default(cluster_id()) -> cluster_id().
cluster_id_or_default(undefined) ->
    riak_cs_config:get_env(riak_cs, default_proxy_cluster_id, undefined);
cluster_id_or_default(ClusterID) ->
    ClusterID.

trim_block_value(RawBlockValue, CurrentBlock,
                 {CurrentBlock, CurrentBlock},
                 {SkipInitial, KeepFinal}) ->
    ValueLength = KeepFinal - SkipInitial,
    <<_Skip:SkipInitial/binary, Value:ValueLength/binary, _Rest/binary>> = RawBlockValue,
    Value;
trim_block_value(RawBlockValue, CurrentBlock,
                 {CurrentBlock, _FinalBlock},
                 {SkipInitial, _KeepFinal}) ->
    <<_Skip:SkipInitial/binary, Value/binary>> = RawBlockValue,
    Value;
trim_block_value(RawBlockValue, CurrentBlock,
                 {_InitialBlock, CurrentBlock},
                 {_SkipInitial, KeepFinal}) ->
    <<Value:KeepFinal/binary, _Rest/binary>> = RawBlockValue,
    Value;
trim_block_value(RawBlockValue, _CurrentBlock,
                 {_InitialBlock, _FinalBlock},
                 {_SkipInitial, _KeepFinal}) ->
    RawBlockValue.

%% ===================================================================
%% Test API
%% ===================================================================

-ifdef(TEST).

test_link(Bucket, Key, ContentLength, BlockSize, FetchConcurrency,
          BufferFactor) ->
    gen_fsm:start_link(?MODULE, [test, Bucket, Key, self(), ContentLength,
                                 BlockSize, FetchConcurrency, BufferFactor],
                       []).

-endif.
