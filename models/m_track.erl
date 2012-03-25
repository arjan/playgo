
-module(m_track).

-behaviour(gen_model).

%% interface functions
-export([
         m_find_value/3,
         m_to_list/2,
         m_value/2,
         install/1,
         update_location/5
]).


-include_lib("zotonic.hrl").


%% @doc Fetch the value for the key from a model source
%% @spec m_find_value(Key, Source, Context) -> term()
m_find_value(Id, M=#m{value=undefined}, _Context) ->
    M#m{value=Id};
m_find_value(tracks, #m{value=Id}, Context) ->
    z_db:assoc("SELECT * FROM track_track WHERE track_id = $1", [Id], Context);
m_find_value(pos, #m{value=Id}, Context) ->
    z_db:assoc("SELECT * FROM track_pos WHERE track_id = $1", [Id], Context).

%% @doc Transform a m_config value to a list, used for template loops
%% @spec m_to_list(Source, Context) -> List
m_to_list(_, _Context) ->
    [].
    
%% @doc Transform a model value so that it can be formatted or piped through filters
%% @spec m_value(Source, Context) -> term()
m_value(#m{value=#m{value={cat, _Id}}}, _Context) ->
    ok.%get(Id, Context).

install(C) ->
    case z_db:table_exists(track_track, C) of
        true -> nop;
        false -> z_db:q("CREATE TABLE track_track (track_id int, ts timestamp, track varchar(255), 
                         artist varchar(255), spotify varchar(255), lat float, long float)", C)
    end,
    case z_db:table_exists(track_pos, C) of
        true -> nop;
        false -> z_db:q("CREATE TABLE track_pos (track_id int, ts timestamp, lat float, long float)", C)
    end.

update_location(TrackId, Ts, Long, Lat, Context) ->
    z_db:q("UPDATE track_track SET long=$1, lat=$2 WHERE track_id=$3 AND ts=$4",
           [Long, Lat, TrackId, Ts], Context).
