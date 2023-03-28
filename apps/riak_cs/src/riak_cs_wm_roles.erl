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

%% @doc WM resource for object listing

-module(riak_cs_wm_roles).

-export([init/1,
         authorize/2,
         stats_prefix/0,
         allowed_methods/0,
         api_request/2
        ]).

-ignore_xref([init/1,
              authorize/2,
              stats_prefix/0,
              allowed_methods/0,
              api_request/2
             ]).

-include("riak_cs.hrl").
-include("riak_cs_api.hrl").
-include("list_objects.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(RIAKCPOOL, bucket_list_pool).

-spec init(#rcs_context{}) -> {ok, #rcs_context{}}.
init(Ctx) ->
    {ok, Ctx#rcs_context{rc_pool=?RIAKCPOOL}}.

-spec stats_prefix() -> list_roles.
stats_prefix() -> list_roles.

-spec allowed_methods() -> [atom()].
allowed_methods() ->
    ['GET'].

-spec authorize(#wm_reqdata{}, #rcs_context{}) -> {boolean(), #wm_reqdata{}, #rcs_context{}}.
authorize(RD, Ctx) ->
    
    true.

-spec api_request(#wm_reqdata{}, #rcs_context{}) -> {ok, ?LORESP{}} | {error, term()}.
api_request(RD, Ctx=#rcs_context{riak_client=RcPid,
                                 user=User}) ->
    UserName = riak_cs_wm_utils:extract_name(User),
    Res = riak_cs_api:list_roles(RcPid),
    Res.
