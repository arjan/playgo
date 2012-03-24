<p>
    Currently recording track: {{ m.rsc[id.current_track].title }}
</p>

{% button text="Stop track" postback={stop_track} delegate=`playmobil` %}

