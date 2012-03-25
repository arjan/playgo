%% @author Arjan Scherpenisse <arjan@scherpenisse.net>
%% @copyright 2012 Arjan Scherpenisse <arjan@scherpenisse.net>
%% Date: 2012-03-24

%% @doc PlAYMOBILE fetcher

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

-module(playmobil_fetch).
-author("Arjan Scherpenisse <arjan@scherpenisse.net>").

-include_lib("include/zotonic.hrl").

-define(CURRENT_LOCATION, "https://www.googleapis.com/latitude/v1/currentLocation").
-define(LOCATION_LIST, "https://www.googleapis.com/latitude/v1/location").


-export([
         collect_all/1,
         collect_track/2,
         collect_position/2,
         stop/2
        ]).


stop(UserId, Context) ->
    TrackId = m_rsc:p(UserId, current_track, Context),

    R = mod_auth_google:request(UserId, "https://www.googleapis.com/oauth2/v1/userinfo", Context),
    ?DEBUG(R),

    
    %% [{data, [{kind,<<"latitude#locationFeed">>},{items, L}]}] = mod_auth_google:request(UserId, ?LOCATION_LIST, Context),

    %% Items = lists:map(fun (Item) ->
    %%                           store_latitude(TrackId, map_latitude_item(Item), Context)
    %%                   end, L),
    %% ?DEBUG(Items),

    %% augment tracks with geo

    augment(TrackId, Context),
    %% mark track done
%    m_rsc:update(UserId, [{current_track, undefined}], Context),
    ok.


augment(TrackId, Context) ->
    Tracks = z_db:assoc("SELECT * FROM track_track WHERE track_id = $1 ORDER BY ts", [TrackId], Context),
    Positions = z_db:assoc("SELECT * FROM track_pos WHERE track_id = $1 ORDER BY ts", [TrackId], Context),

    [augment_song(Track, Positions, Context) || Track <- Tracks],
    ok.

augment_song(Track, Positions, Context) ->
    T = fun(D) -> z_datetime:datetime_to_timestamp(D) end,
    Dt = proplists:get_value(ts, Track),
    Date = T(Dt),

    Last = hd(lists:reverse(Positions)),
    {LastLong, LastLat} = {proplists:get_value(long, Last),proplists:get_value(lat, Last)},
    
    [_|SideBySide] = lists:zip([[]|Positions], Positions++[[]]),

    Interpolate = fun(Delta, X, Y) -> Delta*X + (1-Delta)*Y end,
                      
    Pos = lists:foldr(fun({_, []}, Pos) ->
                              %% last, without pos
                              Pos;
                         ({B, A}, _Pos) ->
                              DateA = T(proplists:get_value(ts, A)),
                              DateB = T(proplists:get_value(ts, B)),
                              case {DateA =< Date, DateB =< Date} of
                                  {false, true} ->
                                      %% inbetween
                                      Delta = (DateB - Date) / (DateB - DateA),
                                      {Interpolate(Delta, proplists:get_value(long, A), proplists:get_value(long, B)),
                                       Interpolate(Delta, proplists:get_value(lat, A), proplists:get_value(lat, B))};
                                  _ ->
                                      _Pos
                              end
                      end,
                      {LastLong, LastLat},
                      SideBySide),

    ?DEBUG(Track),
    ?DEBUG(Pos),
    TrackId = proplists:get_value(track_id, Track),
    {Long, Lat} = Pos,
    z_db:q("UPDATE track_track SET long=$1, lat=$2 WHERE track_id=$3 AND ts=$4",
           [Long, Lat, TrackId, Dt], Context),
    ok.

%% @doc Collect all song + pos for current tracks
collect_all(Context) ->
    R = z_search:query_([{cat, person}], Context),
    lists:map(fun(Id) ->
                      case m_rsc:p(Id, current_track, Context) of
                          undefined -> skip;
                          TrackId -> collect_for_person(Id, TrackId, Context)
                      end
              end, R).


collect_for_person(UserId, TrackId, Context) ->
    %?DEBUG(UserId),
    
    case catch collect_track(UserId, Context) of
        {Artist, Track} ->
            R=mod_lastfm:request_unauthorized("track.getPlayLinks", [{"artist[]", Artist}, {"track[]", Track}], Context),
            %?DEBUG(R),
            %{struct,[{<<"spotify">>,{struct,[{<<"track">>,{struct,[{<<"name">>,<<"Dr. Tyrell's Death">>},{<<"artist">>,{struct,[{<<"#text">>,<<"Vangelis">>},{<<"id">>,<<"1161">>}]}},{<<"externalids">>,{struct,[{<<"spotify">>,<<"spotify:track:3zC88xoyxiITQEvt9FH34a">>}]}},{<<"@attr">>,{struct,[{<<"id">>,<<"151546077">>}]}}]}}]}}]}
            {struct,[{<<"spotify">>,{struct,[{<<"track">>,{struct,P}}]}}]} = R,
            SpotifyLink = case proplists:get_value(<<"externalids">>, P) of
                              {struct, Ext} ->
                                  proplists:get_value(<<"spotify">>, Ext);
                              <<>> ->
                                  undefined
                          end,
            ?DEBUG(SpotifyLink),

            case z_db:q("SELECT MAX(ts) FROM track_track WHERE track_id = $1 AND spotify = $2  ", [TrackId, SpotifyLink], Context) of
                X when X =:= [{undefined}]; X =:= [] ->
                    z_db:insert(track_track, [{track_id, TrackId}, {ts, calendar:local_time()},
                                              {track, Track}, {artist, Artist}, {spotify, SpotifyLink}], Context);
                [{D}] ->
                    ?DEBUG(z_datetime:datetime_to_timestamp(D)),
                    ?DEBUG(z_datetime:datetime_to_timestamp(calendar:local_time())),

                    case (z_datetime:datetime_to_timestamp(calendar:local_time()) - z_datetime:datetime_to_timestamp(D)) < 600 of
                        true ->
                            %% skpi
                            ?DEBUG("SKIP!");
                        false ->
                            % insert
                            z_db:insert(track_track, [{track_id, TrackId}, {ts, calendar:local_time()},
                                                      {track, Track}, {artist, Artist}, {spotify, SpotifyLink}], Context)
                    end
            end;
        R -> ?DEBUG(R)
    end,

    case catch collect_position(UserId, Context) of
        {_,_,_}=Pos ->
            store_latitude(TrackId, Pos, Context);

        R3 -> ?DEBUG(R3)
    end,
    ok.



%% @doc Collect the user's currently listening track according to last.fm
collect_track(UserId, Context) ->
    P = mod_lastfm:request_unauthorized("user.getRecentTracks",
                                        [{user, m_rsc:p(UserId, lastfm_name, Context)},
                                         {limit, 1}],
                                        Context),
    P1 = z_convert:convert_json(P),
    %?DEBUG(P1),
    [{recenttracks, [{track, T}, _]}] = P1,
    TrackEl = case T of
                [T0, _] -> T0;
                X -> X
            end,
    %?DEBUG(TrackEl),
    case proplists:get_value('@attr', TrackEl) of
        [{nowplaying,<<"true">>}] ->
            Artist = text(proplists:get_value(artist, TrackEl)),
            Track = text(proplists:get_value(name, TrackEl)),
            ?DEBUG(Artist),?DEBUG(Track),

            {Artist, Track};
        _R ->
            not_now_playing
    end.


text(B) when is_binary(B) -> B;
text(Elt) -> proplists:get_value('#text', Elt).

%21:38:34.705 [info] DEBUG: playmobil_fetch:106  [{recenttracks,[{track,[[{artist,[{'#text',<<"The Roots">>},{mbid,<<"80b3cf5e-18fe-4c59-98c7-e5bb87210710">>}]},{name,<<"Doin' It Again">>},{streamable,<<"0">>},{mbid,<<>>},{album,[{'#text',<<"How I Got Over">>},{mbid,<<>>}]},{url,<<"http://www.last.fm/music/The+Roots/_/Doin%27+It+Again">>},{image,[[{'#text',<<"http://userserve-ak.last.fm/serve/34s/46802231.png">>},{size,<<"small">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/64s/46802231.png">>},{size,<<"medium">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/126/46802231.png">>},{size,<<"large">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/300x300/46802231.png">>},{size,<<"extralarge">>}]]},{'@attr',[{nowplaying,<<"true">>}]}],[{artist,[{'#text',<<"The Roots">>},{mbid,<<"80b3cf5e-18fe-4c59-98c7-e5bb87210710">>}]},{name,<<"Tunnel Vision">>},{streamable,<<"0">>},{mbid,<<>>},{album,[{'#text',<<"How I Got Over">>},{mbid,<<>>}]},{url,<<"http://www.last.fm/music/The+Roots/_/Tunnel+Vision">>},{image,[[{'#text',<<"http://userserve-ak.last.fm/serve/34s/46802231.png">>},{size,<<"small">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/64s/46802231.png">>},{size,<<"medium">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/126/46802231.png">>},{size,<<"large">>}],[{'#text',<<"http://userserve-ak.last.fm/serve/300x300/46802231.png">>},{size,<<"extralarge">>}]]},{date,[{'#text',<<"24 Mar 2012, 19:43">>},{uts,<<"1332618191">>}]}]]},{'@attr',[{user,<<"acscherp">>},{page,<<"1">>},{perPage,<<"1">>},{totalPages,<<"19822">>},{total,<<"19822">>}]}]}]



%% @doc Collect the user's position according to Google latitude.
collect_position(UserId, Context) ->
%    L = mod_auth_google:request(UserId, ?LOCATION_LIST, Context),
 %   ?DEBUG(L),
    Url = z_convert:to_list(z_html:unescape(m_rsc:p(UserId, google_url, Context))),
    case z_utils:is_empty(Url) of
        false ->
            {ok, {{_, _Code, _}, _, Body}} = httpc:request(get, {Url, []}, [], []),
            {match, [[Lat, Long]]} = re:run(Body, "center=(.*?),(.*?)&", [global, {capture, all_but_first, list}]),
            {calendar:local_time(), z_convert:to_float(Long), z_convert:to_float(Lat)};
        true  ->
            error
    end.


map_latitude_item(P) ->
    TS = z_convert:to_integer(proplists:get_value(timestampMs, P)) div 1000,
    Now = z_datetime:timestamp_to_datetime(TS),
    Long = proplists:get_value(longitude, P),
    Lat = proplists:get_value(latitude, P),
    {Now, Long, Lat}.


store_latitude(TrackId, {Date, Long, Lat}, Context) ->
    Created = m_rsc:p(TrackId, created, Context),
    case Created < Date of
        _ ->
            case z_db:q("SELECT 1 FROM track_pos WHERE track_id = $1 AND long=$2 and lat=$3",
                        [TrackId, Long, Lat], Context) of
                [] ->
                    z_db:insert(track_pos, [{track_id, TrackId}, {ts, Date},
                                            {long, Long}, {lat, Lat}], Context),
                    ok;
                _ ->
                    ign
            end;
        false ->
            date_range
    end.

