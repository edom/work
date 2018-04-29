---
title: All pages
permalink: /all.html
date: 2018-04-28 22:10:00 +0700
---

{% assign everything = site.emptyArray %}
{% for page in site.pages %}
{% assign everything = everything | push: page %}
{% endfor %}
{% assign everything = everything | sort: "title" %}

<ul>
{% for page in everything %}
{% if page.title %}
<li><a href="{{ page.url }}">{{ page.title | escape }}</a></li>
{% endif %}
{% endfor %}
</ul>
