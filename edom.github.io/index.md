---
layout: home
mathjax: yes
draft: yes
---

## Research

See [research]({% link research.md %}).

{% assign everything = site.emptyArray %}
{% for page in site.pages %}
{% assign everything = everything | push: page %}
{% endfor %}
{% assign everything = everything | sort: "title" %}

## Opinions

[Software engineering]({% link software_engineering.md %})

[April fools' day is harmful]({% link april.md %})

## Featured Pages

"Featured" doesn't mean "Complete".
It just means "set apart".

<ul>
{% for page in everything %}
{% if page.featured %}
<li><a href="{{ page.url }}">{{ page.title | escape }}</a></li>
{% endif %}
{% endfor %}
</ul>

## All Pages

<ul>
{% for page in everything %}
{% if page.title %}
<li><a href="{{ page.url }}">{{ page.title | escape }}</a></li>
{% endif %}
{% endfor %}
</ul>
