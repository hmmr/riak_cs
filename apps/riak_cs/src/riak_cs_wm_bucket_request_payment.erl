%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2015 Basho Technologies, Inc.  All Rights Reserved,
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

-module(riak_cs_wm_bucket_request_payment).

-export([stats_prefix/0,
         content_types_provided/2,
         to_xml/2,
         allowed_methods/0
        ]).
-ignore_xref([stats_prefix/0,
              content_types_provided/2,
              to_xml/2,
              allowed_methods/0
             ]).

-export([authorize/2]).

-include("riak_cs.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-spec stats_prefix() -> bucket_request_payment.
stats_prefix() -> bucket_request_payment.

%% @doc Get the list of methods this resource supports.
% TODO: no PUT support yet
-spec allowed_methods() -> [atom()].
allowed_methods() ->
    ['GET'].

-spec content_types_provided(#wm_reqdata{}, #rcs_web_context{}) -> {[{string(), atom()}], #wm_reqdata{}, #rcs_web_context{}}.
content_types_provided(RD, Ctx) ->
    {[{"application/xml", to_xml}], RD, Ctx}.

-spec authorize(#wm_reqdata{}, #rcs_web_context{}) ->
          {boolean() | {halt, non_neg_integer()}, #wm_reqdata{}, #rcs_web_context{}}.
authorize(RD, Ctx) ->
    riak_cs_wm_utils:bucket_access_authorize_helper(bucket_request_payment, false, RD, Ctx).

-spec to_xml(#wm_reqdata{}, #rcs_web_context{}) ->
                    {binary() | {'halt', term()}, #wm_reqdata{}, #rcs_web_context{}}.
to_xml(RD, Ctx = #rcs_web_context{user = User,
                                  bucket = Bucket}) ->
    StrBucket = binary_to_list(Bucket),
    case [B || B <- riak_cs_bucket:get_buckets(User),
               B?RCS_BUCKET.name =:= StrBucket] of
        [] ->
            riak_cs_aws_response:api_error(no_such_bucket, RD, Ctx);
        [_BucketRecord] ->
            Doc = [{'RequestPaymentConfiguration',
                    [{xmlns, "http://s3.amazonaws.com/doc/2006-03-01/"}],
                    [{'Payer', [<<"BucketOwner">>]}]}],
            {riak_cs_xml:to_xml(Doc), RD, Ctx}
    end.
