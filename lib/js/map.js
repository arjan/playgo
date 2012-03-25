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
            mapTypeId: google.maps.MapTypeId.ROADMAP,
            disableDefaultUI: true
        };
        this.map = new google.maps.Map(this.mapContainer, mapOpts);
    },

    addPoints: function() {
        var marker, markerDragEnd, title;

        $(points).each($.proxy(function(i) {
            var point = this.points[i];
            title = point.artist + " - " + point.track + " @ " + point.ts;
            marker = this.addMarker(i, point.lat, point.long, title);

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

    addMarker: function(i, lat, long, title) {
        var iconURL = './lib/images/mid.png';

        if (i === 0) {
            iconURL = './lib/images/start.png';
        } else if (i === this.points.length - 1) {
            iconURL = './lib/images/end.png';
        }

        var icon = new google.maps.MarkerImage(iconURL,
            new google.maps.Size(30, 30),
            new google.maps.Point(0, 0),
            new google.maps.Point(15, 15));

        return new google.maps.Marker({
            position: new google.maps.LatLng(lat, long),
            map: this.map,
            draggable: true,
            icon: icon,
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

        console.log('This point', data.point, 'was moved to', data.newpos);
    }

});