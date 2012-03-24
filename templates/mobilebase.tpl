<!DOCTYPE html>
<html>
    <head>
        <title>{% block title %}Upload cinema{% endblock %}</title>

        {% lib "css/jquery.mobile-1.0b3.min.css"
        %}
{#        <link rel="stylesheet" href="/lib/css/{% block color %}blue{% endblock %}.css" /> #}
        {% lib "css/jquery.loadmask.css" %}

        <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />

        <meta name="apple-mobile-web-app-capable" content="yes" />
        <meta name="apple-mobile-web-app-status-bar-style" content="black" />

        {% lib "js/jquery-1.6.2.min.js" "js/jquery.mobile-1.0b3.min.js" "js/touch-scroll.min.js" "js/cinema.js" %}
    </head>
    <body>

        {% lib
        "js/apps/zotonic-1.0.js"
        "js/apps/z.widgetmanager.js"
        "js/modules/jquery.loadmask.js"
        %}
        {% block _js_include_extra %}{% endblock %}
        {% stream %}

        {% block entire_page %}
        <div data-role="page">

            <div data-role="header" id="header-top">
                {% block header %}
                <h1><span>{% block title %}{% endblock %}</span></h1>
                {% endblock %}
            </div>

            <div data-role="content">
                {% block content_area %}
                {% block content %}
                <p>The content</p>
                {% endblock %}
                {% endblock %}
            </div>

            {% script %}
        </div>
        {% endblock %}

        <script>$(document).ready(function(){$('#header-top').touchScroll();});</script>

        {% all include "_html_body.tpl" %}

    </body>
</html>
