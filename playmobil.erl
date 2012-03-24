%% @author Arjan Scherpenisse
%% @copyright 2012 Arjan Scherpenisse
%% Generated on 2012-03-24
%% @doc This site was based on the 'empty' skeleton.

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

-module(playmobil).
-author("Arjan Scherpenisse").

-mod_title("playmobil zotonic site").
-mod_description("An empty Zotonic site, to base your site on.").
-mod_prio(10).

-include_lib("zotonic.hrl").

-export([
         observe_signup_url/2,
         event/2
        ]).

-define(CURRENT_LOCATION, "https://www.googleapis.com/latitude/v1/currentLocation").

%%====================================================================
%% support functions go here
%%====================================================================

text(Elt) ->
    proplists:get_value('#text', Elt).

event(#postback{message={get_location, _A}}, Context) ->
    R = collect_position(z_acl:user(Context), Context),
    ?DEBUG(R),

    Context;

event(#postback{message={get_song, _A}}, Context) ->
    collect_track(z_acl:user(Context), Context),
    Context.



    

    %[{recenttracks,[{track,[[{artist,[{'#text',<<"Yellowjackets">>},{mbid,<<"1243d99e-8f5a-4f7e-810c-5a1d20f41065">>}]},{name,<<"Revelation - Live (1991 The Roxy)">>},{streamable,<<"0">>},{mbid,<<>>},{album,[{'#text',<<>>},{mbid,<<>>}]},{url,<<"http://www.last.fm/music/Yellowjackets/_/Revelation+-+Live+%281991+The+Roxy%29">>},{image,[[{'#text',<<>>},{size,<<"small">>}],[{'#text',<<>>},{size,<<"medium">>}],[{'#text',<<>>},{size,<<"large">>}],[{'#text',<<>>},{size,<<"extralarge">>}]]},{'@attr',[{nowplaying,<<"true">>}]}],[{artist,[{'#text',<<"ScHoolboy Q">>},{mbid,<<"bce6d667-cde8-485e-b078-c0a05adea36d">>}]},{name,<<"Hands On The Wheel (feat. A$ap Rocky)">>},{streamable,<<"0">>},{mbid,<<>>},{album,[{'#text',<<"Habits & Contradictions">>},{mbid,<<>>}]},{url,<<"http://www.last.fm/music/ScHoolboy+Q/_/Hands+On+The+Wheel+%28feat.+A%24ap+Rocky%29">>},{image,[[{'#text',<<>>},{size,<<"small">>}],[{'#text',<<>>},{size,<<"medium">>}],[{'#text',<<>>},{size,<<"large">>}],[{'#text',<<>>},{size,<<"extralarge">>}]]},{date,[{'#text',<<"24 Mar 2012, 11:49">>},{uts,<<"1332589777">>}]}]]},{'@attr',[{user,<<"acscherp">>},{page,<<"1">>},{perPage,<<"1">>},{totalPages,<<"19797">>},{total,<<"19797">>}]}]}]

%% %    ?DEBUG(P),
%%     {struct, Tracks} = proplists:get_value(<<"recenttracks">>, P),
%%     %{struct, Track} = proplists:get_value(<<"track">>, Tracks),
%%     ?DEBUG(Tracks),



observe_signup_url(#signup_url{props=Props, signup_props=SignupProps}, Context) ->
    {ok, UserId} = mod_signup:signup(Props, SignupProps, false, Context),
    z_auth:logon(UserId, z_context:ensure_all(Context)),
    {ok, m_rsc:p(UserId, page_url, Context)}.



%% @doc Collect the user's currently listening track according to last.fm
collect_track(UserId, Context) ->
    P = mod_lastfm:request_unauthorized("user.getRecentTracks",
                                        [{user, m_rsc:p(UserId, lastfm_name, Context)},
                                         {limit, 1}],
                                        Context),
    P1 = z_convert:convert_json(P),
    [{recenttracks, [{track, [T|_]}, _]}] = P1,

    Artist = text(proplists:get_value(artist, T)),
    Track = proplists:get_value(name, T),
    {Artist, Track}.
    

%% @doc Collect the user's position according to Google latitude.
collect_position(UserId, Context) ->
    [{data, P}] = mod_auth_google:request(UserId, ?CURRENT_LOCATION, Context),
    ?DEBUG(P),
    TS = z_convert:to_integer(proplists:get_value(timestampMs, P)) div 1000,
    Now = calendar:universal_time_to_local_time(z_datetime:timestamp_to_datetime(TS)),
    Long = proplists:get_value(longitude, P),
    Lat = proplists:get_value(latitude, P),
    {Now, Long, Lat}.
