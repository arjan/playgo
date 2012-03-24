{% if id.lastfm_name %}
<h2>Step 1: OK</h2>
<p>You have authorized with lastfm as <u>{{ id.lastfm_name }}</u></h1>
{% else %}
<h2>Step 1</h2>
<p>Please sign in with last.fm to begin</p>
{% button text="Logon to last.fm" action={redirect dispatch="lastfm_authorize" p=page} %}
{% endif %}

{% if id.google_token %}
<h2>Step 2: OK</h1>
<p>You have authorized this app to use you location.</p>
<p>When you are ready, start your track by clicking below.</p>


{% button text="Start a new track!" postback={start_track} delegate=`playmobil` %}

{% else %}
{% button text="Logon to google" action={redirect dispatch="auth_google_authorize" p=page} %}
{% endif %}


