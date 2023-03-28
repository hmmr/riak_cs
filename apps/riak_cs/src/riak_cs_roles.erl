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

%% @doc riak_cs bucket utility functions, but I dare not use a module
%% name with '_utils'.

-module(riak_cs_roles).

-export([create_role/2,
         delete_role/2,
         get_role/2,
         list_roles/1
        ]).

-include("riak_cs.hrl").
-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-include_lib("riakc/include/riakc.hrl").
-include_lib("kernel/include/logger.hrl").

create_role(_RcPid, Specs) ->
    logger:debug("STUB create_role(~p)", [Specs]).

delete_role(_RcPid, RoleId) ->
    logger:debug("STUB delete_role(~p)", [RoleId]).

get_role(_RcPid, RoleId) ->
    logger:debug("STUB get_role(~p)", [RoleId]).

list_roles(_RcPid) ->
    logger:debug("STUB list_roles", []).


-ifdef(TEST).
-compile([export_all, nowarn_export_all]).
-include_lib("eunit/include/eunit.hrl").
-endif.
