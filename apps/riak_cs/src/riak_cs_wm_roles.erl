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

%% @doc WM resource for listing roles

-module(riak_cs_wm_roles).

-export([allowed_methods/0,
         api_request/2
        ]).

-ignore_xref([allowed_methods/0,
              api_request/2
             ]).

-include("riak_cs.hrl").
-include("riak_cs_api.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-spec allowed_methods() -> [atom()].
allowed_methods() ->
    ['GET'].

-spec api_request(#wm_reqdata{}, #rcs_s3_context{}) -> {ok, ?LRRESP{}} | {error, term()}.
api_request(_RD, _Ctx) ->
    Res = riak_cs_api:list_roles(),
    Res.
