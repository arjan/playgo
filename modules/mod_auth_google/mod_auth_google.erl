%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-03-24

%% @doc Authenticate to Google using OAuth2

%% Copyright 2012 Arjan Scherpenisse
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%%     http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(mod_auth_google).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Google Authentication").
-mod_description("Authenticate to Google using OAuth2").
-mod_prio(900).

-include_lib("include/zotonic.hrl").

-export([
         authorize_uri/1,
         retrieve_access_token/2,
         request/3
        ]).

-define(AUTH_URL, "https://accounts.google.com/o/oauth2/auth").
-define(TOKEN_URL, "https://accounts.google.com/o/oauth2/token").
-define(ASSERTION_TYPE, "http://oauth.net/grant_type/jwt/1.0/bearer").


request(UserId, Url, Context) ->
    Token = m_rsc:p(UserId, google_token, Context),
    {ok, {{_, Code, _}, _, Body}} = httpc:request(get, {Url, [{"Authorization", "Bearer " ++ z_convert:to_list(Token)}]}, [], []),
    Payload = z_convert:convert_json(mochijson2:decode(Body)),
    case Code of
        200 ->
            Payload;
        403 ->
            ?DEBUG(Payload),
            error;
        503 ->
            ?DEBUG(Payload),
            error;
        401 ->
            %% extend code
            ?DEBUG(Payload),

%%            ok = renew_token(UserId, RenewToke, Context),
            %%          request(UserId, Url, Context)
            ok
    end.
    

%% @doc Return the URI at which to authorize at google.
authorize_uri(Context) ->
    {Scope, Id, _Sec} = params(Context),
    RedirectUrl = redirect_uri(Context),
    Args = [{scope, Scope},
            {client_id, Id},
            {redirect_uri, RedirectUrl},
            {response_type, code}
           ],
    ?AUTH_URL ++ "?" ++ implode_query(Args).

redirect_uri(Context) ->
    lists:flatten(z_context:abs_url(z_dispatcher:url_for(auth_google_redirect, [], Context), Context)).

retrieve_access_token(Code, Context) ->
    {_Scope, Id, Secret} = params(Context),
    Args = [{code, Code},
            {grant_type, authorization_code},
            {redirect_uri, redirect_uri(Context)},
            {client_id, Id},
            {client_secret, Secret}],
    Body = implode_query(Args),
    {ok, {_, _, Response}} = httpc:request(post, {?TOKEN_URL, [], "application/x-www-form-urlencoded", Body}, [], []),
    mochijson2:decode(Response).


%% @doc Get the authorization params
params(Context) ->
    {m_config:get_value(?MODULE, scope, Context),
     m_config:get_value(?MODULE, client_id, Context),
     m_config:get_value(?MODULE, client_secret, Context)}.


implode_query(Args) ->
    [_|Query] = lists:flatten(lists:foldr(fun({K, V}, Acc) ->
                                                  ["&", z_utils:url_encode(z_convert:to_list(K)), "=",
                                                   z_utils:url_encode(z_convert:to_list(V)) | Acc] end,
                                          [], Args)),
    Query.
