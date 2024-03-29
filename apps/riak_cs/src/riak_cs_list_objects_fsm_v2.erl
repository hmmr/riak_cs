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

%% TODO:
%% 1. maybe `objects' should be called `manifests'

-module(riak_cs_list_objects_fsm_v2).

-behaviour(gen_fsm).

-include("riak_cs.hrl").
-include("riak_cs_web.hrl").
-include_lib("kernel/include/logger.hrl").

%%%===================================================================
%%% Exports
%%%===================================================================

-ifdef(TEST).
-compile(export_all).
-compile(nowarn_export_all).
-endif.

%% API
-export([start_link/2,
         start_link/3]).

%% Observability
-export([]).

%% gen_fsm callbacks
-export([init/1,
         prepare/2,
         waiting_object_list/2,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%%===================================================================
%%% Records and Types
%%%===================================================================

-record(profiling, {
        temp_fold_objects_request :: undefined |
                                     {Request :: {StartKey :: binary(),
                                                  EndKey :: binary()},
                                      StartTime :: non_neg_integer()},
        fold_objects_requests=[] :: [{Request :: {StartKey :: binary(),
                                                  EndKey :: binary()},
                                      NumKeysReturned :: non_neg_integer(),
                                      Timing :: {StartTime :: non_neg_integer(),
                                                 EndTime :: non_neg_integer()}}]}).

-type profiling() :: #profiling{}.


-record(state, {riak_client :: pid(),
                req :: list_object_request(),
                reply_ref :: undefined | {pid(), any()},
                fold_objects_batch_size :: pos_integer(),
                object_list_req_id :: undefined | reference(),
                reached_end_of_keyspace=false :: boolean(),
                object_buffer=[] :: list(),
                objects=[] :: list(),
                last_request_start_key :: undefined | binary(),
                last_request_num_keys_requested :: undefined | pos_integer(),
                object_list_ranges=[] :: object_list_ranges(),
                profiling=#profiling{} :: profiling(),
                response :: undefined |
                            {ok, list_objects_response() | list_object_versions_response()} |
                            {error, term()},
                common_prefixes=ordsets:new() :: list_objects_common_prefixes()}).

%% some useful shared types

-type state() :: #state{}.

-type fsm_state_return() :: {next_state, atom(), state()} |
                            {next_state, atom(), state(), non_neg_integer()} |
                            {stop, term(), state()}.

-type continuation() :: binary() | 'undefined'.
-type list_objects_event() :: {ReqID :: reference(), {done, continuation()}} |
                              {ReqID :: reference(), {ok, list()}} |
                              {ReqID :: reference(), {error, term()}}.

%% `Start' and `End' are inclusive
-type object_list_range()  :: {Start :: binary(), End :: binary()}.
-type object_list_ranges() :: list(object_list_range()).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link(riak_client(), list_object_request()) ->
    {ok, pid()} | {error, term()}.
start_link(RcPid, ListKeysRequest) ->
    FoldObjectsBatchSize = 1002,
    start_link(RcPid, ListKeysRequest, FoldObjectsBatchSize).

-spec start_link(riak_client(), list_object_request(), pos_integer()) ->
    {ok, pid()} | {error, term()}.
start_link(RcPid, ListKeysRequest, FoldObjectsBatchSize) ->
    BatchSize2 = max(2, FoldObjectsBatchSize),
    gen_fsm:start_link(?MODULE, [RcPid, ListKeysRequest, BatchSize2], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================

-spec init(list()) -> {ok, prepare, state(), 0}.
init([RcPid, Request, FoldObjectsBatchSize]) ->
    State = #state{riak_client=RcPid,
                   fold_objects_batch_size=FoldObjectsBatchSize,
                   req=Request},
    {ok, prepare, State, 0}.

-spec prepare(timeout, state()) -> fsm_state_return().
prepare(timeout, State=#state{riak_client=RcPid}) ->
    case make_2i_request(RcPid, State) of
        {NewStateData, {ok, ReqId}} ->
            {next_state, waiting_object_list,
             NewStateData#state{object_list_req_id=ReqId}};
        {NewStateData, {error, _Reason}=Error} ->
            try_reply(Error, NewStateData)
    end.

-spec waiting_object_list(list_objects_event(), state()) -> fsm_state_return().
waiting_object_list({ReqId, {ok, ObjectList}}, State = #state{object_list_req_id = ReqId,
                                                              object_buffer = ObjectBuffer}) ->
    NewStateData = State#state{object_buffer = ObjectBuffer ++ ObjectList},
    {next_state, waiting_object_list, NewStateData};
waiting_object_list({ReqId, {done, _Continuation}}, State=#state{object_list_req_id=ReqId}) ->
    handle_done(State);
waiting_object_list({ReqId, {error, _Reason}=Error},
                    State=#state{object_list_req_id=ReqId}) ->
    handle_error(Error, State).

handle_event(_Event, StateName, State) ->
    %% TODO: log unknown event
    {next_state, StateName, State}.

handle_sync_event(get_object_list, From, StateName,
                  State=#state{response=undefined}) ->
    NewStateData = State#state{reply_ref=From},
    {next_state, StateName, NewStateData};
handle_sync_event(get_object_list, _From, _StateName,
                  State=#state{response=Resp}) ->
    {stop, normal, Resp, State};
handle_sync_event(get_internal_state, _From, StateName, State) ->
    Reply = {StateName, State},
    {reply, Reply, StateName, State};
handle_sync_event(Event, _From, StateName, State) ->
    logger:warning("got unknown event ~p in state ~p", [Event, StateName]),
    Reply = ok,
    {reply, Reply, StateName, State}.

%% the responses from `riakc_pb_socket:get_index_range'
%% come back as regular messages, so just pass
%% them along as if they were gen_server events.
handle_info(Info, waiting_object_list, State) ->
    waiting_object_list(Info, State);
handle_info(Info, StateName, _State) ->
    logger:warning("Received unknown info message ~p in state ~p", [Info, StateName]),
    ok.

terminate(normal, _StateName, State) ->
    ?LOG_DEBUG(format_profiling_from_state(State));
terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%====================================================================
%%% Internal helpers
%%%====================================================================

handle_done(State=#state{object_buffer=ObjectBuffer,
                         objects=PrevObjects,
                         last_request_num_keys_requested=NumKeysRequested,
                         common_prefixes=CommonPrefixes,
                         req=Request}) ->

    ObjectBufferLength = length(ObjectBuffer),
    RangeUpdatedStateData =
        update_profiling_and_last_request(State, ObjectBuffer, ObjectBufferLength),

    FilteredObjects = exclude_key_from_state(State, ObjectBuffer),
    Manifests = [riak_cs_manifest:manifests_from_riak_object(O) ||
                    O <- FilteredObjects],

    Active = map_active_manifests(Manifests),
    NewObjects = PrevObjects ++ Active,
    ObjectPrefixTuple = {NewObjects, CommonPrefixes},

    {NewManis, NewPrefixes} =
        riak_cs_list_objects_utils:filter_prefix_keys(ObjectPrefixTuple, Request),

    ReachedEnd = reached_end_of_keyspace(ObjectBufferLength,
                                         NumKeysRequested,
                                         Active,
                                         Request?LOREQ.prefix),

    NewStateData = RangeUpdatedStateData#state{objects=NewManis,
                                               common_prefixes=NewPrefixes,
                                               reached_end_of_keyspace=ReachedEnd,
                                               object_buffer=[]},
    respond(NewStateData, NewManis, NewPrefixes).

reached_end_of_keyspace(BufferLength, NumKeysRequested, _, _)
  when BufferLength < NumKeysRequested ->
    true;
reached_end_of_keyspace(_, _, _ActiveObjects, undefined) ->
    false;
reached_end_of_keyspace(_BufferLength, _NumKeysRequested, [], _) ->
    false;
reached_end_of_keyspace(_, _, ActiveObjects, Prefix) ->
    M = lists:last(ActiveObjects),
    {_, LastKey} = M?MANIFEST.bkey,
    PrefixLen = byte_size(Prefix),
    case LastKey of
        << Prefix:PrefixLen/binary, _/binary >> ->
            false;
        _ ->
            LastKey > Prefix
    end.

handle_error(Error, #state{profiling=Profiling} = State) ->
    {_KeyRange, StartTime} = Profiling#profiling.temp_fold_objects_request,
    _ = riak_cs_stats:update_error_with_start([riakc, fold_manifest_objs], StartTime),
    try_reply(Error, State).

update_profiling_and_last_request(State, ObjectBuffer, ObjectBufferLength) ->
    State2 = update_profiling_state_with_end(State, os:system_time(millisecond),
                                             ObjectBufferLength),
    update_last_request_state(State2, ObjectBuffer).

respond(StateData=#state{req=Request=?LOREQ{max_keys=UserMaxKeys,
                                            delimiter=Delimiter}},
        Manifests, Prefixes) ->
    case enough_results(StateData) of
        true ->
            Truncated = truncated(UserMaxKeys, {Manifests, Prefixes}),
            SlicedTaggedItems =
            riak_cs_list_objects_utils:manifests_and_prefix_slice({Manifests, Prefixes},
                                                                  UserMaxKeys),
            NextMarker = next_marker(Delimiter, SlicedTaggedItems),

            {NewManis, NewPrefixes} =
            riak_cs_list_objects_utils:untagged_manifest_and_prefix(SlicedTaggedItems),
            Response =
                response_from_manifests_and_common_prefixes(
                  Request, Truncated, NextMarker, {NewManis, NewPrefixes}),
            try_reply({ok, Response}, StateData);
        false ->
            RcPid = StateData#state.riak_client,
            case make_2i_request(RcPid, StateData) of
                {NewStateData2, {ok, ReqId}} ->
                    {next_state, waiting_object_list,
                     NewStateData2#state{object_list_req_id=ReqId}};
                {NewStateData2, {error, _Reason}=Error} ->
                    try_reply(Error, NewStateData2)
            end
    end.

truncated(NumKeysRequested, ObjectsAndPrefixes) ->
    NumKeysRequested < riak_cs_list_objects_utils:manifests_and_prefix_length(ObjectsAndPrefixes) andalso
    %% this is because (strangely) S3 returns `false' for
    %% `isTruncated' if `max-keys=0', even if there are more keys.
    %% The `Ceph' tests were nice to find this.
    NumKeysRequested =/= 0.

%% @doc Return a `boolean' determining whether enough results have been
%% returned from the fold objects queries to return to the user. In order
%% to tell if the result-set is truncated, we either need one more result
%% than the user has asked for (hence `>' and not `>='), or to know
%% that we've already reached the end of the fold objects results.
enough_results(#state{req=?LOREQ{max_keys=UserMaxKeys},
                      reached_end_of_keyspace=EndOfKeyspace,
                      objects=Objects,
                      common_prefixes=CommonPrefixes}) ->
    riak_cs_list_objects_utils:manifests_and_prefix_length({Objects, CommonPrefixes})
    > UserMaxKeys
    orelse EndOfKeyspace.

next_marker(undefined, _List) ->
    undefined;
next_marker(_Delimiter, []) ->
    undefined;
next_marker(_Delimiter, List) ->
    next_marker_from_element(lists:last(List)).

next_marker_from_element({prefix, Name}) ->
    Name;
next_marker_from_element({manifest, ?MANIFEST{bkey={_Bucket, Key}}}) ->
    Key;
next_marker_from_element({manifest, {Key, ?MANIFEST{}}}) ->
    Key.

response_from_manifests_and_common_prefixes(?LOREQ{req_type = ReqType} = Request,
                                            Truncated,
                                            NextMarker,
                                            {Manifests, CommonPrefixes}) ->
    KeyContent = lists:map(fun(M) -> riak_cs_list_objects:manifest_to_keycontent(ReqType, M) end,
                           Manifests),
    riak_cs_list_objects:new_response(Request, Truncated, NextMarker,
                                      CommonPrefixes,
                                      KeyContent).

make_2i_request(RcPid, State=#state{req = ?LOREQ{name = BucketName, prefix = Prefix},
                                    fold_objects_batch_size = BatchSize}) ->
    ManifestBucket = riak_cs_utils:to_bucket_name(objects, BucketName),
    StartKey = make_start_key(State),
    EndKey = riak_cs_utils:big_end_key(Prefix),

    NewStateData = State#state{last_request_start_key=StartKey,
                               last_request_num_keys_requested=BatchSize},
    NewStateData2 = update_profiling_state_with_start(NewStateData,
                                                      StartKey,
                                                      EndKey,
                                                      os:system_time(millisecond)),
    Opts = [{max_results, BatchSize},
            {start_key, StartKey},
            {end_key, EndKey},
            {timeout, riak_cs_config:fold_objects_timeout()}],
    {ok, ManifestPbc} = riak_cs_riak_client:manifest_pbc(RcPid),
    FoldResult = riakc_pb_socket:cs_bucket_fold(ManifestPbc,
                                                ManifestBucket,
                                                Opts),
    {NewStateData2, FoldResult}.

last_result_is_common_prefix(#state{object_list_ranges=Ranges,
                                    req=Request}) ->
    Key = element(2, lists:last(Ranges)),
    key_is_common_prefix(Key, Request).

key_is_common_prefix(_Key, ?LOREQ{delimiter=undefined}) ->
    false;
key_is_common_prefix(Key, ?LOREQ{prefix=Prefix,
                                 delimiter=Delimiter}) ->
    case Prefix of
        undefined ->
            handle_undefined_prefix(Key, Delimiter);
        _Prefix ->
            handle_prefix(Key, Prefix, Delimiter)
    end.

handle_undefined_prefix(Key, Delimiter) ->
    case binary:match(Key, [Delimiter]) of
        nomatch ->
            false;
        _Match ->
            true
    end.

handle_prefix(Key, Prefix, Delimiter) ->
    PrefixLen = byte_size(Prefix),
    case Key of
        <<Prefix:PrefixLen/binary, Rest/binary>> ->
            case binary:match(Rest, [Delimiter]) of
                nomatch ->
                    false;
                _Match ->
                    true
            end;
        _NoPrefix ->
            false
    end.

%% @doc Extract common prefix from `Key'. `Key' must contain `Delimiter', so
%% you must first check with `key_is_common_prefix'.
common_prefix_from_key(Key, Prefix, Delimiter) ->
    case Prefix of
        undefined ->
            riak_cs_list_objects_utils:extract_group(Key, Delimiter);
        _Prefix ->
            PrefixLen = byte_size(Prefix),
            << Prefix:PrefixLen/binary, Rest/binary >> = Key,
            Group = riak_cs_list_objects_utils:extract_group(Rest, Delimiter),
            <<Prefix/binary, Group/binary>>
    end.

make_start_key(#state{object_list_ranges=[], req=Request}) ->
    make_start_key_from_marker_and_prefix(Request);
make_start_key(State=#state{object_list_ranges=PrevRanges,
                            common_prefixes=CommonPrefixes,
                            req=?LOREQ{prefix=Prefix,
                                       delimiter=Delimiter}}) ->
    Key = element(2, lists:last(PrevRanges)),
    case last_result_is_common_prefix(State) of
        true ->
            LastPrefix = common_prefix_from_key(Key, Prefix, Delimiter),
            case ordsets:is_element(LastPrefix, CommonPrefixes) of
                true ->
                    skip_past_prefix_and_delimiter(LastPrefix);
                false ->
                    Key
            end;
        false ->
            Key
    end.

make_start_key_from_marker_and_prefix(?LOREQ{marker=undefined,
                                             prefix=undefined}) ->
    <<0:8/integer>>;
make_start_key_from_marker_and_prefix(?LOREQ{marker=undefined,
                                             prefix=Prefix}) ->
    Prefix;
make_start_key_from_marker_and_prefix(?LOREQ{marker=Marker,
                                             delimiter=undefined}) ->
    Marker;
make_start_key_from_marker_and_prefix(?LOREQ{marker=Marker,
                                             delimiter=Delimiter}) ->
    DelSize = byte_size(Delimiter),
    case binary:longest_common_suffix([Marker, Delimiter]) of
        DelSize ->
            %% when the `Marker' itself ends with the delimiter,
            %% then we should skip past the entire set of keys
            %% that would be rolled up into that common delimiter
            skip_past_prefix_and_delimiter(Marker);
        _Else ->
            Marker
    end.

map_active_manifests(Manifests) ->
    ActiveTuples = [rcs_common_manifest_utils:active_manifest(M) ||
            M <- Manifests],
    [A || {ok, A} <- ActiveTuples].

exclude_key_from_state(_State, []) ->
    [];
exclude_key_from_state(#state{object_list_ranges=[],
                              req=Request}, Objects) ->
    exclude_marker(Request, Objects);
exclude_key_from_state(#state{last_request_start_key=StartKey}, Objects) ->
    exclude_key(StartKey, Objects).

exclude_marker(?LOREQ{marker=undefined}, Objects) ->
    Objects;
exclude_marker(?LOREQ{marker=Marker}, Objects) ->
    exclude_key(Marker, Objects).

exclude_key(Key, [H | T]=Objects) ->
    case riakc_obj:key(H) == Key of
        true ->
            T;
        false ->
            Objects
    end.

skip_past_prefix_and_delimiter(<<>>) ->
    <<0:8/integer>>;
skip_past_prefix_and_delimiter(Key) ->
    PrefixSize = byte_size(Key) - 1,
    <<Prefix:PrefixSize/binary, LastByte/binary>> = Key,
    NextByte = next_byte(LastByte),
    <<Prefix/binary, NextByte/binary>>.

-spec next_byte(binary()) -> binary().
next_byte(<<Integer:8/integer>>=Byte) when Integer == 255 ->
    Byte;
next_byte(<<Integer:8/integer>>) ->
    <<(Integer+1):8/integer>>.

try_reply(Response, State) ->
    NewStateData = State#state{response=Response},
    reply_or_wait(Response, NewStateData).

reply_or_wait(_Response, State=#state{reply_ref=undefined}) ->
    {next_state, waiting_req, State};
reply_or_wait(Response, State=#state{reply_ref=Ref}) ->
    gen_fsm:reply(Ref, Response),
    Reason = make_reason(Response),
    {stop, Reason, State}.

make_reason({ok, _Response}) ->
    normal;
make_reason({error, Reason}) ->
    Reason.

update_last_request_state(State=#state{last_request_start_key=StartKey,
                                       object_list_ranges=PrevRanges},
                          []) ->
    NewRange = {StartKey, StartKey},
    State#state{object_list_ranges=PrevRanges ++ [NewRange]};
update_last_request_state(State=#state{last_request_start_key=StartKey,
                                       object_list_ranges=PrevRanges},
                          RiakObjects) ->
    LastObject = lists:last(RiakObjects),
    LastKey = riakc_obj:key(LastObject),
    NewRange = {StartKey, LastKey},
    State#state{object_list_ranges=PrevRanges ++ [NewRange]}.

%% Profiling helper functions
%%--------------------------------------------------------------------

update_profiling_state_with_start(State=#state{profiling=Profiling},
                                  StartKey, EndKey, StartTime) ->
    _ = riak_cs_stats:inflow([riakc, fold_manifest_objs]),
    TempData = {{StartKey, EndKey},
                StartTime},
    NewProfiling = Profiling#profiling{temp_fold_objects_request=TempData},
    State#state{profiling=NewProfiling}.

update_profiling_state_with_end(State=#state{profiling=Profiling},
                                EndTime, NumKeysReturned) ->
    {KeyRange, StartTime} = Profiling#profiling.temp_fold_objects_request,
    _ = riak_cs_stats:update_with_start([riakc, fold_manifest_objs], StartTime),
    OldRequests = Profiling#profiling.fold_objects_requests,
    NewRequest = {KeyRange, NumKeysReturned, {StartTime, EndTime}},
    NewProfiling = Profiling#profiling{temp_fold_objects_request=undefined,
                                       fold_objects_requests=
                                           [NewRequest | OldRequests]},
    State#state{profiling=NewProfiling}.

extract_timings(Requests) ->
    [extract_timing(R) || R <- Requests].

%% TODO: time to make legit types out of these
extract_timing({_Range, NumKeysReturned, {StartTime, EndTime}}) ->
    MillisecondDiff = EndTime - StartTime,
    {MillisecondDiff, NumKeysReturned}.

format_profiling_from_state(#state{req = Request,
                                   response = {ok, Response},
                                   profiling = Profiling}) ->
    format_profiling(Request, Response, Profiling, self()).

format_profiling(Request, ?LORESP{contents = Contents, common_prefixes = CommonPrefixes},
                 Profiling, Pid) ->
    format_profiling(Request, Contents, CommonPrefixes, Profiling, Pid);
format_profiling(Request, ?LOVRESP{contents = Contents, common_prefixes = CommonPrefixes},
                 Profiling, Pid) ->
    format_profiling(Request, Contents, CommonPrefixes, Profiling, Pid).

format_profiling(?LOREQ{max_keys = MaxKeys},
                 Contents, CommonPrefixes,
                 #profiling{fold_objects_requests = Requests},
                 Pid) ->
    string:join([io_lib:format("~p: User requested ~p keys", [Pid, MaxKeys]),
                 io_lib:format("~p: We returned ~p objects",
                               [Pid, length(Contents)]),
                 io_lib:format("~p: We returned ~p common prefixes",
                               [Pid, ordsets:size(CommonPrefixes)]),
                 io_lib:format("~p: With fold objects timings: {Millis, NumObjects}: ~p",
                               %% We reverse the Requests in here because they
                               %% were cons'd as they happened.
                               [Pid, extract_timings(lists:reverse(Requests))])],
                io_lib:nl()).
