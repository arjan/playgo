{% if m.config.mod_lastfm.api_key.value %}
<li id="logon_lastfm">
    <a id="{{ #lastfm_logon }}" href="#lastfm"><img src="/lib/images/lastfm.png" width="32" height="32" alt="{_ Sign in with Lastfm _}" /></a>
    {% wire id=#lastfm_logon 
	action={mask target=mask_target|default:"logon_outer" message=_"Waiting for Lastfm…"}
	action={redirect dispatch="lastfm_authorize" p=page}
    %}
</li>
{% endif %}
