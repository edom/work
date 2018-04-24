---
layout: home
mathjax: yes
draft: yes
---

- What do you do?
    - [Research]({% link research.md %})
- What are some of your opinions?
    - [Software engineering]({% link software_engineering.md %})
    - [April fools' day is harmful]({% link april.md %})
- How do I ask questions or suggest corrections?
    - [Open an issue on GitHub](https://github.com/edom/edom.github.io/issues).
- What can you teach me?
    - [Building a mental model for using Git]({% link git.md %})
    - [Friendship is measured by sacrifice]({% link friendship.md %})
    - [How to cook eggs]({% link egg.md %})

{% assign everything = site.emptyArray %}
{% for page in site.pages %}
{% assign everything = everything | push: page %}
{% endfor %}
{% assign everything = everything | sort: "title" %}

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
