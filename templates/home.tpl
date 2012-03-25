{% extends "mobilebase.tpl" %}

{% block content %}

<h1>Welcome to PlayGo</h1>

{% if not m.acl.user %}
    {% button text="Logon with last.fm"
       action={redirect dispatch="lastfm_authorize"}
    %}
    {% else %}
    {% button text="Go to your page"
       action={redirect location=m.acl.user.page_url}
    %}
    
{% endif %}

{% endblock %}

