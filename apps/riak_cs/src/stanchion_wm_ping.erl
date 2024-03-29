%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved,
%%               2022, 2023 TI Tokyo    All Rights Reserved.
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

-module(stanchion_wm_ping).

-export([init/1,
         service_available/2,
         allowed_methods/2,
         to_html/2,
         finish_request/2
        ]).

-ignore_xref([init/1,
              service_available/2,
              allowed_methods/2,
              to_html/2,
              finish_request/2
             ]).

-include("riak_cs_web.hrl").

-record(ping_context, {pool_pid = true :: boolean(),
                       riak_client :: undefined | pid()}).

%% -------------------------------------------------------------------
%% Webmachine callbacks
%% -------------------------------------------------------------------

init(_Config) ->
    {ok, #ping_context{}}.

-spec service_available(#wm_reqdata{}, #ping_context{}) -> {boolean(), #wm_reqdata{}, #ping_context{}}.
service_available(RD, Ctx) ->
    {Available, UpdCtx} = riak_ping(get_connection_pid(), Ctx),
    {Available, RD, UpdCtx}.

-spec allowed_methods(term(), term()) -> {[atom()], term(), term()}.
allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD'], RD, Ctx}.

to_html(ReqData, Ctx) ->
    {"OK", ReqData, Ctx}.

finish_request(RD, Ctx=#ping_context{riak_client=undefined}) ->
    {true, RD, Ctx};
finish_request(RD, Ctx=#ping_context{riak_client=RcPid,
                                     pool_pid=PoolPid}) ->
    case PoolPid of
        true ->
            riak_cs_riak_client:checkin(RcPid);
        false ->
            riak_cs_riak_client:stop(RcPid)
    end,
    {true, RD, Ctx#ping_context{riak_client=undefined}}.

%% -------------------------------------------------------------------
%% Internal functions
%% -------------------------------------------------------------------

get_connection_pid() ->
    case pool_checkout() of
        full ->
            non_pool_connection();
        RcPid ->
            {RcPid, true}
    end.

pool_checkout() ->
    case riak_cs_riak_client:checkout(request_pool) of
        {ok, RcPid} ->
            RcPid;
        {error, _Reason} ->
            full
    end.

non_pool_connection() ->
    case riak_cs_riak_client:start_link([]) of
        {ok, RcPid} ->
            {RcPid, false};
        {error, _} ->
            {undefined, false}
    end.

riak_ping({RcPid, PoolPid}, Ctx) ->
    {ok, MasterPbc} = riak_cs_riak_client:master_pbc(RcPid),
    Timeout = riak_cs_config:ping_timeout(),
    Available = case catch riak_cs_pbc:ping(MasterPbc, Timeout, [riakc, ping]) of
                    pong ->
                        true;
                    _ ->
                        false
                end,
    {Available, Ctx#ping_context{riak_client=RcPid, pool_pid=PoolPid}}.
