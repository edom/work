---
title: All
permalink: /all.html
date: 2017-06-27 02:00:00 +0700
---

{% assign everything = site.emptyArray %}
{% for page in site.pages %}
{% assign everything = everything | push: page %}
{% endfor %}
{% for post in site.posts %}
{% assign everything = everything | push: post %}
{% endfor %}
{% assign everything = everything | sort: "title" %}

{% for page in everything %}
{% if page.title %}
- [{{ page.title | escape }}]({{ page.url }})
{% endif %}
{% endfor %}
