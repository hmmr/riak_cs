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

-module(riak_cs_sp_helper).

-export([process_specs/0]).

process_specs() ->
    case riak_cs_config:is_saml_enabled() of
        false ->
            [];
        true  ->
            [#{id => riak_cs_sp_sup,
               start => {riak_cs_sp_sup, start_link, []},
               type => supervisor,
               modules => dynamic}
            ]
    end.

