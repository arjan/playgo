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

{% with m.track[id].pos as points %}
{% if points %}

<div id="map" style="width: 100%; height: 500px"></div>

<script>
      var map;
      function initialize() {
          var points = {{ points|to_json }};

          var myOptions = {
          zoom: 17,
          center: new google.maps.LatLng(points[0].lat, points[0].long),
          mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        map = new google.maps.Map(document.getElementById('map'),
        myOptions);
        for (var i=0; i<points.length; i++) {
            var m = new google.maps.Marker({position: new google.maps.LatLng(points[i].lat, points[i].long), map: map, title: points[i].ts});
            }
      }

      //google.maps.event.addDomListener(window, 'load', initialize);
      initialize();
  </script>
{% else %}

{% button text="Delete this track!!" postback={remove_track id=id} delegate=`playmobil` %}
  
{% endif %}
{% endwith %}



{% endblock %}
