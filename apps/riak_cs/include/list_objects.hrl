%% ---------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved,
%%               2021 TI Tokyo    All Rights Reserved.
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

%% see also: http://docs.amazonwebservices.com/AmazonS3/latest/API/RESTBucketGET.html
%% non mandatory keys have `| undefined' as a
%% type option.

-type list_objects_req_type() :: objects | versions.

-record(list_objects_request,
        {
         req_type :: list_objects_req_type(),

         %% the name of the bucket
         name :: binary(),

         %% how many keys to return in the response
         max_keys :: non_neg_integer(),

         %% a 'starts-with' parameter
         prefix :: binary() | undefined,

         %% a binary to group keys by
         delimiter :: binary() | undefined,

         %% the key and version_id to start with
         marker :: binary() | undefined,
         version_id_marker :: binary() | undefined
        }).
-type list_object_request() :: #list_objects_request{}.
-define(LOREQ, #list_objects_request).

-type next_marker() :: 'undefined' | binary().

-record(list_objects_response,
        {
         %% Params just echoed back from the request --------------------------

         %% the name of the bucket
         name :: binary(),

         %% how many keys were requested to be
         %% returned in the response
         max_keys :: non_neg_integer(),

         %% a 'starts-with' parameter
         prefix :: binary() | undefined,

         %% a binary to group keys by
         delimiter :: binary() | undefined,

         %% the marker used in the _request_
         marker :: binary() | undefined,
         %% the (optional) marker to use for pagination
         %% in the _next_ request
         next_marker :: next_marker(),
         %% (marker and next_marker not used in ListObjectsV2)

         %% The actual response -----------------------------------------------
         is_truncated :: boolean(),

         contents :: [list_objects_key_content()],

         common_prefixes :: list_objects_common_prefixes()
        }).

-type list_objects_response() :: #list_objects_response{}.
-define(LORESP, #list_objects_response).

-record(list_objects_key_content,
        {
         key :: binary(),
         last_modified :: term(),
         etag :: binary(),
         size :: non_neg_integer(),
         owner :: list_objects_owner(),
         storage_class :: binary()
        }).
-type list_objects_key_content() :: #list_objects_key_content{}.
-define(LOKC, #list_objects_key_content).


-record(list_object_versions_response,
        {
         %% Params just echoed back from the request --------------------------

         %% the name of the bucket
         name :: binary(),

         %% how many keys were requested to be
         %% returned in the response
         max_keys :: non_neg_integer(),

         %% a 'starts-with' parameter
         prefix :: binary() | undefined,

         %% a binary to group keys by
         delimiter :: binary() | undefined,

         %% the marker used in the _request_
         key_marker :: binary() | undefined,
         version_id_marker :: binary() | undefined,

         %% the (optional) marker to use for pagination
         %% in the _next_ request
         next_key_marker :: next_marker(),
         next_version_id_marker :: next_marker(),

         %% The actual response -----------------------------------------------
         is_truncated :: boolean(),

         contents :: [list_object_versions_key_content()],

         common_prefixes :: list_objects_common_prefixes()
        }).

-type list_object_versions_response() :: #list_object_versions_response{}.
-define(LOVRESP, #list_object_versions_response).

-record(list_object_versions_key_content,
        {
         key :: binary(),
         last_modified :: term(),
         etag :: binary(),
         is_latest :: boolean(),
         version_id :: binary(),
         size :: non_neg_integer(),
         owner :: list_objects_owner(),
         storage_class :: binary()
        }).
-type list_object_versions_key_content() :: #list_object_versions_key_content{}.
-define(LOVKC, #list_object_versions_key_content).

-record(list_objects_owner, {
        id :: binary(),
        display_name :: binary()}).
-type list_objects_owner() :: #list_objects_owner{}.

-type list_objects_common_prefixes() :: list(binary()).

-define(LIST_OBJECTS_CACHE, list_objects_cache).
-define(ENABLE_CACHE, true).
-define(CACHE_TIMEOUT, timer:minutes(15)).
-define(MIN_KEYS_TO_CACHE, 2000).
-define(MAX_CACHE_BYTES, 104857600). % 100MB
-define(KEY_LIST_MULTIPLIER, 1.1).
-define(FOLD_OBJECTS_FOR_LIST_KEYS, true).
