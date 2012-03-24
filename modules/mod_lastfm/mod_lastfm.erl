%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-03-24

%% @doc Last.fm interaction

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

-module(mod_lastfm).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-mod_title("Last.fm").
-mod_description("Last.fm login and API access").
-mod_prio(900).

-define(URL_BASE, "http://ws.audioscrobbler.com/2.0/").

-include_lib("include/zotonic.hrl").

-export([authorize_url/1, api_sig/2, request/3]).

authorize_url(Context) ->
    "http://www.last.fm/api/auth/?api_key=" ++ z_convert:to_list(m_config:get_value(?MODULE, api_key, Context)).


request(Method, Args, Context) ->

    Args1 = [{method, Method}, {api_key, m_config:get_value(?MODULE, api_key, Context)} | Args],
    Sig = api_sig(Args1, Context),
    Args2 = [{api_sig, Sig} | Args1],
    [_|Query] = lists:flatten(lists:foldr(fun({K, V}, Acc) ->
                                ["&", z_utils:url_encode(z_convert:to_list(K)), "=",
                                 z_utils:url_encode(z_convert:to_list(V)) | Acc] end,
                        [], Args2)),
    Url = ?URL_BASE ++ "?" ++ Query ++ "&format=json",
    {ok, {_, _, Body}} = httpc:request(Url),
    mochijson2:decode(Body).



api_sig(Args, Context) ->
    Secret = z_convert:to_list(m_config:get_value(?MODULE, api_secret, Context)),
    Sorted = lists:sort([[z_convert:to_list(K), z_convert:to_list(V)] || {K,V} <- Args]),
    ?DEBUG(Sorted),
    Sig = md5_hex([Sorted, Secret]),
    ?DEBUG(Sig),
    Sig.


md5_hex(S) ->
    Md5_bin =  erlang:md5(iolist_to_binary(S)),
    Md5_list = binary_to_list(Md5_bin),
    lists:flatten(list_to_hex(Md5_list)).

list_to_hex(L) ->
    lists:map(fun(X) -> int_to_hex(X) end, L).

int_to_hex(N) when N < 256 ->
    [hex(N div 16), hex(N rem 16)].

hex(N) when N < 10 ->
    $0+N;
hex(N) when N >= 10, N < 16 ->
    $a + (N-10).
