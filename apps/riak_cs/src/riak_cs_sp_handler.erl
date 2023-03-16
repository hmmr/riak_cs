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

-module(riak_cs_sp_handler).

-include_lib("esaml/include/esaml.hrl").

-record(state, {sp, idp}).
-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, _Args) ->
    %% Load the certificate and private key for the SP
    PrivKey = esaml_util:load_private_key(
                riak_cs_config:saml_sp_privkey()),
    Cert = esaml_util:load_certificate(
             riak_cs_config:saml_sp_cert()),
    %% We build all of our URLs (in metadata, and in requests) based on this
    Base = "http://localhost/saml",
    %% Certificate fingerprints to accept from our IDP
    FPs = riak_cs_config:saml_idp_cert_fingerprints(),

    SP = esaml_sp:setup(#esaml_sp{key = PrivKey,
                                  certificate = Cert,
                                  trusted_fingerprints = FPs,
                                  consume_uri = Base ++ "/consume",
                                  metadata_uri = Base ++ "/metadata",
                                  org = #esaml_org{name = riak_cs_config:saml_sp_org_name(),
                                                   displayname = riak_cs_config:saml_sp_org_displayname(),
                                                   url = riak_cs_config:saml_sp_org_url()
                                                  },
                                  tech = #esaml_contact{name = "Foo Bar",
                                                        email = "foo@bar.com"
                                                       }
                                 }),
    %% Rather than copying the IDP's metadata into our code, we'll just fetch it
    %% (this call will cache after the first time around, so it will be fast)
    IdpMeta = esaml_util:load_metadata(
                riak_cs_config:saml_idp_metadata_url()),

    {ok, Req, #state{sp = SP, idp = IdpMeta}}.

handle(Req, S = #state{}) ->
    {Operation, Req2} = cowboy_req:binding(operation, Req),
    {Method, Req3} = cowboy_req:method(Req2),
    handle(Method, Operation, Req3, S).

                                                % Return our SP metadata as signed XML
handle(<<"GET">>, <<"metadata">>, Req, S = #state{sp = SP}) ->
    {ok, Req2} = esaml_cowboy:reply_with_metadata(SP, Req),
    {ok, Req2, S};

                                                % Visit /saml/auth to start the authentication process -- we will make an AuthnRequest
                                                % and send it to our IDP
handle(<<"GET">>, <<"auth">>, Req, S = #state{sp = SP,
                                              idp = #esaml_idp_metadata{login_location = IDP}}) ->
    {ok, Req2} = esaml_cowboy:reply_with_authnreq(SP, IDP, <<"foo">>, Req),
    {ok, Req2, S};

                                                % Handles HTTP-POST bound assertions coming back from the IDP.
handle(<<"POST">>, <<"consume">>, Req, S = #state{sp = SP}) ->
    case esaml_cowboy:validate_assertion(SP, fun esaml_util:check_dupe_ets/2, Req) of
        {ok, Assertion, RelayState, Req2} ->
            Attrs = Assertion#esaml_assertion.attributes,
            Uid = proplists:get_value(uid, Attrs),
            Output = io_lib:format("<html><head><title>SAML SP demo</title></head><body><h1>Hi there!</h1><p>This is the <code>esaml_sp_default</code> demo SP callback module from eSAML.</p><table><tr><td>Your name:</td><td>\n~p\n</td></tr><tr><td>Your UID:</td><td>\n~p\n</td></tr></table><hr /><p>RelayState:</p><pre>\n~p\n</pre><p>The assertion I got was:</p><pre>\n~p\n</pre></body></html>", [Assertion#esaml_assertion.subject#esaml_subject.name, Uid, RelayState, Assertion]),
            {ok, Req3} = cowboy_req:reply(200, [{<<"Content-Type">>, <<"text/html">>}], Output, Req2),
            {ok, Req3, S};

        {error, Reason, Req2} ->
            {ok, Req3} = cowboy_req:reply(403, [{<<"content-type">>, <<"text/plain">>}],
                                          ["Access denied, assertion failed validation:\n", io_lib:format("~p\n", [Reason])],
                                          Req2),
            {ok, Req3, S}
    end;

handle(_, _, Req, S = #state{}) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<"Not found">>, Req),
    {ok, Req2, S}.

terminate(_Reason, _Req, _State) -> ok.
