<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"     "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
    <dict>
        <key>title</key>
        <string>{{ id.title }}</string>
        <key>creator</key>
        <string>{{ id.creator_id.title }}</string>
        <key>date</key>
        <string>{{ id.created|date:"Y-m-dTH:i:s+0200" }}</string>
        <key>playlist</key>
        <array>
            {% for row in m.track[id].tracks %}
            <dict>
                <key>lat</key>
                <real>{{ row.lat }}</real>
                <key>long</key>
                <real>{{ row.long }}</real>
                <key>artist</key>
                <string>{{ row.artist|escape }}</string>
                <key>track</key>
                <string>{{ row.track|escape }}</string>
                <key>spotify</key>
                <string>{{ row.spotify|escape }}</string>
            </dict>
            {% endfor %}
        </array>
    </dict>
</plist>

