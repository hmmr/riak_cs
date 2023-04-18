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

-module(riak_cs_aws_rewrite).
-behaviour(riak_cs_rewrite).

-export([rewrite/5,
         original_resource/1,
         raw_url/1
        ]).

-include("riak_cs.hrl").
-include("s3_api.hrl").
-include_lib("kernel/include/logger.hrl").


aws_service_submodule(?S3_ROOT_HOST) -> riak_cs_aws_s3_rewrite;
aws_service_submodule(?IAM_ROOT_HOST) -> riak_cs_aws_iam_rewrite.

-spec rewrite(atom(), atom(), {integer(), integer()}, mochiweb_headers(), string()) ->
                     {mochiweb_headers(), string()}.
rewrite(Method, Scheme, Vsn, Headers, Url) ->
    Mod = aws_service_submodule(root_host(Url)),
    Mod:rewrite(Method, Scheme, Vsn, Headers, Url).

root_host(Url) ->
    #{path := Path} = uri_string:parse(Url),
    case lists:search(
           fun(A) -> re:run(Path, A++"$") /= nomatch end,
           [?S3_ROOT_HOST, ?IAM_ROOT_HOST]) of
        {value, Found} ->
            Found;
        false ->
            logger:warning("Request URL (\"~s\") not recognized as any AWS service", [Url]),
            bad_aws_host
    end.

-spec original_resource(#wm_reqdata{}) -> undefined | {string(), [{term(),term()}]}.
original_resource(RD) ->
    riak_cs_rewrite:original_resource(RD).

-spec raw_url(#wm_reqdata{}) -> undefined | {string(), [{term(), term()}]}.
raw_url(RD) ->
    riak_cs_rewrite:raw_url(RD).
