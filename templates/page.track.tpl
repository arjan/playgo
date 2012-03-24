{% extends "mobilebase.tpl" %}

{% block title %}
Track: {{ id.title }}
{% endblock %}

{% block content %}
<p>
    Created on {{ id.created|date:"j F Y @ H:i" }} by
    <a href="{{ id.creator_id.page_url }}">{{ id.creator_id.title }}</a>
</p>

<h3>Tracks:</h3>

<ul data-role="listview" data-inset="true" >
    {% for row in m.track[id].tracks %}
    <li>
        <a rel="external" href="{{ row.spotify }}">{{ row.artist }} &mdash; {{ row.track }} at {{ row.ts|date:"H:i" }}</a>
    </li>
    {% endfor %}
</ul>

<script type="text/javascript" src="//maps.googleapis.com/maps/api/js?sensor=false"></script>

<div id="map" style="width: 100%; height: 500px"></div>

<script>
      var map;
      function initialize() {
        var myOptions = {
          zoom: 8,
          center: new google.maps.LatLng(-34.397, 150.644),
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        map = new google.maps.Map(document.getElementById('map_canvas'),
            myOptions);
      }

      google.maps.event.addDomListener(window, 'load', initialize);
//      map.init({{ m.track[id].pos|to_json }});
</script>
AIzaSyCRaWRbcCzpWMtczsCS2vzrSMdVf2wkU8o
{% button text="delete" postback={remove_track id=id} delegate=`playmobil` %}

{% endblock %}
