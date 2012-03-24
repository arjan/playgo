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
-behaviour(gen_server).

-mod_title("playmobil zotonic site").
-mod_description("Aggregating geo sound.").
-mod_prio(10).

-include_lib("zotonic.hrl").

-record(state, {context}).

%% gen_server exports
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([start_link/1]).

-export([
         observe_signup_url/2,
         event/2
        ]).


%%====================================================================
%% API
%%====================================================================

event(#postback{message={get_location, _A}}, Context) ->
    R = playmobil_fetch:collect_position(z_acl:user(Context), Context),
    ?DEBUG(R),
    Context;

event(#postback{message={remove_track, [{id, Id}]}}, Context) ->
    m_rsc:delete(Id, Context),
    UserId = z_acl:user(Context),
    z_render:wire({redirect, [{location, m_rsc:p(UserId, page_url, Context)}]}, Context);

event(#postback{message={get_song, _A}}, Context) ->
    T = playmobil_fetch:collect_track(z_acl:user(Context), Context),
    ?DEBUG(T),
    Context;

event(#postback{message={start_track, _A}}, Context) ->
    ?DEBUG(_A),
    Id = z_acl:user(Context),
    {ok, TrackId} = m_rsc:insert([{category, track},
                                  {is_published, true},
                                  {title, "Unknown track"}],
                                 Context),
    m_rsc:update(Id, [{current_track, TrackId}], Context),
    z_render:wire({redirect, [{dispatch, auth_google_authorize}]}, Context);

event(#postback{message={stop_track, _A}}, Context) ->
    Id = z_acl:user(Context),
    %TrackId = m_rsc:p(Id, current_track, Context),
    m_rsc:update(Id, [{current_track, undefined}], Context),
    z_render:wire({reload, []}, Context).



observe_signup_url(#signup_url{props=Props, signup_props=SignupProps}, Context) ->
    {ok, UserId} = mod_signup:signup(Props, SignupProps, false, Context),
    z_auth:logon(UserId, z_context:ensure_all(Context)),
    {ok, m_rsc:p(UserId, page_url, Context)}.


%% @spec start_link() -> {ok,Pid} | ignore | {error,Error}
%% @doc Starts the server

start_link(Args) when is_list(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init(Args) ->
    ?DEBUG("init playmobil"),
    {context, Context} = proplists:lookup(context, Args),
    m_track:install(Context),
    timer:send_interval(15000, poll),
    {ok, #state{context=Context}}.

handle_call(Message, _From, State) ->
    {stop, {unknown_call, Message}, State}.


handle_cast(Message, State) ->
    {stop, {unknown_cast, Message}, State}.


handle_info(poll, State=#state{context=Context}) ->
    playmobil_fetch:collect_all(Context),
    {noreply, State};

handle_info(_Info, State) ->
    ?DEBUG(_Info),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%====================================================================
%% support functions
%%====================================================================

