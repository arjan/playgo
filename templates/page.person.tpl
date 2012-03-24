{% extends "page.tpl" %}

{% block content %}
<h1>{{ id.title }}</h1>
{% debug %}

{% print m.acl.user %}

{% if id.lastfm_name %}
<h1>Authorized with lastfm as <u>{{ id.lastfm_name }}</u></h1>

{% button text="Get current song" postback={get_song} delegate=`playmobil` %}

{% endif %}

{% if id.google_token %}
<h1>Authorized at google</h1>

<div id="loc"></div>
{% button text="Get my location" postback={get_location} delegate=`playmobil` %}

{% else %}
{% endif %}
{% button text="Logon to google" action={redirect dispatch="auth_google_authorize" p=page} %}

{% endblock %}

