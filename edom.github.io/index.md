---
layout: default
date: 2017-05-20 00:00:00 +0700
---

# Home

- What do you do?
    - [Research]({% link research.md %})
    - Music
        - I play piano in the band [The Nomads]({% link nomads.md %}) (an Indonesian band, not the Swedish band).
- How do I ask questions or suggest corrections?
    - Any of these will do:
        - Leave a Disqus comment in the related page.
        - [Open an issue on GitHub](https://github.com/edom/edom.github.io/issues).
- Where is everything?
    - See [All pages]({% link all.md %}).
- How do you use a computer?
    - [Internet search tools]({% link search.md %})
    - [Remove nag screens]({% link nag.md %})
- Why is the text so big?
    - Because I'm myopic.
    - How do I shrink the text?
        - Try pressing Control-minus.
- [Similar websites]({% link similar.md %})
- Some private things you might not care about
    - Habits learned the hard way
        - I check the time at [time.gov](https://time.gov/) because Ubuntu 14.04 NTP once betrayed me.
    - Vital service failure log
        - [Fastnet]({% link fastnet.md %})
        - [PLN]({% link pln.md %})

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
