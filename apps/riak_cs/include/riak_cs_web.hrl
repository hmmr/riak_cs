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

-ifndef(RIAK_CS_WEB_HRL).
-define(RIAK_CS_WEB_HRL, included).

-include("manifest.hrl").
-include("moss.hrl").
-include("s3_api.hrl").

-type api() :: s3 | oos.

-record(rcs_s3_context, {start_time :: undefined | erlang:timestamp(),
                         auth_bypass :: atom(),
                         user :: undefined | moss_user(),
                         user_object :: undefined | riakc_obj:riakc_obj(),
                         role :: undefined | role(),
                         bucket :: undefined | binary(),
                         acl :: 'undefined' | acl(),
                         requested_perm :: undefined | acl_perm(),
                         riak_client :: undefined | pid(),
                         rc_pool :: atom(),    % pool name which riak_client belongs to
                         auto_rc_close = true :: boolean(),
                         submodule :: module(),
                         exports_fun :: undefined | function(),
                         auth_module :: module(),
                         response_module :: module(),
                         policy_module :: module(),
                         %% Key for API rate and latency stats.
                         %% If `stats_prefix' or `stats_key' is `no_stats', no stats
                         %% will be gathered by riak_cs_wm_common.
                         %% The prefix is defined by `stats_prefix()' callback of sub-module.
                         %% If sub-module provides only `stats_prefix' (almost the case),
                         %% stats key is [Prefix, HttpMethod]. Otherwise, sum-module
                         %% can set specific `stats_key' by any callback that returns
                         %% this context.
                         stats_prefix = no_stats :: atom(),
                         stats_key = prefix_and_method :: prefix_and_method |
                                                          no_stats |
                                                          riak_cs_stats:key(),
                         local_context :: term(),
                         api :: atom()
                        }).

-record(rcs_iam_context, {start_time :: undefined | erlang:timestamp(),
                          auth_bypass :: atom(),
                          user :: undefined | moss_user(),
                          role :: undefined | role(),
                          riak_client :: undefined | pid(),
                          rc_pool :: atom(),    % pool name which riak_client belongs to
                          auto_rc_close = true :: boolean(),
                          exports_fun :: undefined | function(),
                          submodule :: module(),
                          auth_module :: module(),
                          response_module :: module(),
                          stats_prefix = no_stats :: atom(),
                          stats_key = prefix_and_method :: prefix_and_method |
                                                           no_stats |
                                                           riak_cs_stats:key(),
                          api :: atom()
                         }).


-record(key_context, {manifest :: undefined | 'notfound' | lfs_manifest(),
                      upload_id :: undefined | binary(),
                      part_number :: undefined | integer(),
                      part_uuid :: undefined | binary(),
                      get_fsm_pid :: undefined | pid(),
                      putctype :: undefined | string(),
                      bucket :: undefined | binary(),
                      bucket_object :: undefined | notfound | riakc_obj:riakc_obj(),
                      key :: undefined | binary(),
                      obj_vsn = ?LFS_DEFAULT_OBJECT_VERSION :: binary(),
                      owner :: undefined | string(),
                      size :: undefined | non_neg_integer(),
                      content_md5 :: undefined | binary(),
                      update_metadata = false :: boolean()}).

-record(access_v1, {
          method :: 'PUT' | 'GET' | 'POST' | 'DELETE' | 'HEAD',
          target :: atom(), % object | object_acl | ....
          id :: string(),
          bucket :: binary(),
          key = <<>> :: undefined | binary(),
          req %:: #wm_reqdata{} % request of webmachine
         }).
-type access() :: #access_v1{}.

-type mochiweb_headers() :: gb_trees:tree().

-define(JSON_TYPE, "application/json").
-define(XML_TYPE, "application/xml").

-endif.
