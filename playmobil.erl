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
         observe_signup_url/2
        ]).

%%====================================================================
%% support functions go here
%%====================================================================


observe_signup_url(#signup_url{props=Props, signup_props=SignupProps}, Context) ->
?DEBUG("Ready1"),
    {ok, UserId} = mod_signup:signup(Props, SignupProps, false, Context),
    ContextUser = z_auth:logon(UserId, z_context:ensure_all(Context)),
    {ok, m_rsc:p(UserId, page_url, Context)}.
