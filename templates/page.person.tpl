{% extends "page.tpl" %}

{% block content %}
<h1>{{ id.title }}</h1>
{% debug %}

{% print m.acl.user %}

{% if id.lastfm_name %}
<h1>Authorized with lastfm as <u>{{ id.lastfm_name }}</u></h1>
{% endif %}

{% endblock %}

