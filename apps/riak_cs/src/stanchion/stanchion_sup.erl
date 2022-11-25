%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2022 TI Tokyo, All Rights Reserved.
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

%% @doc Supervisor for the stanchion process tree.

-module(stanchion_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).
-export([stanchion_process_specs/0]).


-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


-spec init([]) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
    {ok, Mode} = application:get_env(riak_cs, operation_mode),
    {ok, Pbc} = riak_connection(),
    ThisHostAddr = riak_cs_utils:this_host_addr(),
    Children =
        case stanchion_migration:do_we_get_to_run_stanchion(Mode, ThisHostAddr, Pbc) of
            {use_saved, HostPort} ->
                ok = stanchion_migration:apply_stanchion_details(HostPort),
                [];
            use_ours ->
                {ok, {_IP, Port}} = application:get_env(riak_cs, stanchion_listener),
                ok = stanchion_migration:save_stanchion_data(Pbc, {ThisHostAddr, Port}),
                stanchion_process_specs()
        end,
    ok = riakc_pb_socket:stop(Pbc),
    {ok, {#{strategy => one_for_one,
            intensity => 10,
            period => 10}, Children
         }}.


stanchion_process_specs() ->
    {ok, {Ip, Port}} = application:get_env(riak_cs, stanchion_listener),

    %% Hide any bags from user-facing parts.
    case application:get_env(riak_cs, supercluster_members) of
        undefined -> ok;
        {ok, Bags} -> application:set_env(riak_cs, bags, Bags)
    end,

    WebConfig1 = [{dispatch, stanchion_web:dispatch_table()},
                  {ip, Ip},
                  {port, Port},
                  {nodelay, true},
                  {log_dir, "log"},
                  %% {rewrite_module, stanchion_wm_rewrite},
                  {error_handler, stanchion_wm_error_handler}
                 ],
    WebConfig =
        case application:get_env(riak_cs, stanchion_ssl) of
            {ok, true} ->
                {ok, CF} = application:get_env(riak_cs, stanchion_ssl_certfile),
                {ok, KF} = application:get_env(riak_cs, stanchion_ssl_keyfile),
                WebConfig1 ++ [{ssl, true},
                               {ssl_opts, [{certfile, CF}, {keyfile, KF}]}];
            {ok, false} ->
                WebConfig1
        end,
    Web =
        #{id => stanchion_webmachine,
          start => {webmachine_mochiweb, start, [WebConfig]},
          restart => permanent,
          shutdown => 5000,
          modules => dynamic},
    ServerSup =
        #{id => stanchion_server_sup,
          start => {stanchion_server_sup, start_link, []},
          restart => permanent,
          shutdown => 5000,
          type => supervisor,
          modules => dynamic},
    [ServerSup, Web].

riak_connection() ->
    {Host, Port} = riak_cs_config:riak_host_port(),
    Timeout = case application:get_env(riak_cs, riakc_connect_timeout) of
                  {ok, ConfigValue} ->
                      ConfigValue;
                  undefined ->
                      10000
              end,
    StartOptions = [{connect_timeout, Timeout},
                    {auto_reconnect, true}],
    riakc_pb_socket:start_link(Host, Port, StartOptions).