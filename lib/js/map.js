var PlayGo = function(points, mapContainer) {
    this.points = points;
    this.mapContainer = $(mapContainer)[0];
    this.initialize();
};

$.extend(PlayGo.prototype, {

    initialize: function() {
        this.initializeMap();
        this.addPoints();
        this.drawPolyLine();
    },

    initializeMap: function() {
        var mapOpts = {
            zoom: 15,
            center: new google.maps.LatLng(this.points[0].lat, this.points[0].long),
            mapTypeId: google.maps.MapTypeId.ROADMAP
        };
        this.map = new google.maps.Map(this.mapContainer, mapOpts);
    },

    addPoints: function() {
        var marker, markerDragEnd, points = this.points, point, title;

        $(points).each($.proxy(function(i) {
            point = points[i];
            title = point.artist + " - " + point.track + " @ " + point.ts;
            marker = this.addMarker(point.lat, point.long, title);

            markerDragEnd = $.proxy(function(marker) {
                this.markerDragEnd(i, point, marker);
            }, this);

            google.maps.event.addListener(marker, 'dragend', markerDragEnd);
        }, this));
    },

    drawPolyLine: function() {
        var latLngs = [], line;

        if (this.line) {
            this.line.setMap(null);
        }

        for (var i = 0; i < this.points.length; i++) {
            latLngs.push(new google.maps.LatLng(this.points[i].lat, this.points[i].long));
        }

        line = new google.maps.Polyline({
          path: latLngs,
          strokeColor: "#f3cb36",
          strokeOpacity: 1.0,
          strokeWeight: 6
        });

        line.setMap(this.map);

        return this.line = line;
    },

    updatePolyLine: function(i, latLng) {
        this.points[i].lat = latLng.lat;
        this.points[i].long = latLng.long;
        this.line.setMap(null);
        this.drawPolyLine();
    },

    addMarker: function(lat, long, title) {
        return new google.maps.Marker({
            position: new google.maps.LatLng(lat, long),
            map: this.map,
            draggable: true,
            title: title
        });
    },

    markerDragEnd: function(i, point, marker) {
        var data = {
            point: point,
            newpos: {
                lat: marker.latLng.lat(),
                long: marker.latLng.lng()
            }
        };

        this.updatePolyLine(i, data.newpos);

        z_event("marker_move", {
                    ts: data.point.ts,
                    track_id: data.point.track_id,
                    long: data.newpos.long,
                    lat: data.newpos.lat
                });
        console.log('This point', data.point, 'was moved to', data.newpos);
    }

});