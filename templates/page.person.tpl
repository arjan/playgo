{% extends "mobilebase.tpl" %}

{% block title %}
{{ m.rsc[m.acl.user].title }}'s page
{% endblock %}

{% block content %}

<h1>Tracks by this user:</h1>
<ul>
    {% for id in m.search[{query creator_id=id cat=`track`}] %}
    <li><a href="{{ id.page_url }}">{{ id.title }}</a> created on {{ id.created|date:"j F Y" }}</li>
    {% endfor %}
</ul>

{% if m.acl.user == id %}
<h1>Welcome on your page</h1>

{% if id.current_track %}
{% include "_user_stop.tpl" %}
{% else %}
{% include "_user_start.tpl" %}
{% endif %}
{#
{% button text="Get current song" postback={get_song} delegate=`playmobil` %}
{% button text="Get my location" postback={get_location} delegate=`playmobil` %}
#}
{% endif %}
{% endblock %}

