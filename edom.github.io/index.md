---
layout: default
date: 2017-05-20 00:00:00 +0700
---

# Home

- What do you do?
    - [Research]({% link research.md %})
    - [Music]({% link music.md %})
- How do I ask questions or suggest corrections?
    - Any of these will do:
        - Leave a Disqus comment in the related page.
        - [Open an issue on GitHub](https://github.com/edom/edom.github.io/issues).
- What can you teach me?
    - [Building a mental model for using Git]({% link git.md %})
    - [Friendship is measured by sacrifice]({% link friendship.md %})
    - [How to cook eggs]({% link egg.md %})
    - [Remove nag screens]({% link nag.md %})
- What are some of your opinions?
    - [Software engineering]({% link software_engineering.md %})
    - [April fools' day is harmful]({% link april.md %})
- Where is everything?
    - See [All pages]({% link all.md %}).
- Why is the text so big?
    - Because I'm myopic.
    - In Chrome and Firefox, you can shrink the text with Control-minus.
- Keeping score
    - [Fastnet]({% link fastnet.md %})
    - [PLN]({% link pln.md %})
- Graveyard
    - [Seaweed]({% link seaweed.md %})
    - [Walking]({% link walk.md %})

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
