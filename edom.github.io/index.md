---
layout: default
date: 2017-05-20 00:00:00 +0700
---

# Home

- *This website may contain mistakes.*
Use with caution.
Corroborate statements with other independent sources.
- [Research]({% link research.md %})
- Music
    - I play piano in the band [The Nomads]({% link nomads.md %}) (an Indonesian band, not the Swedish band).
- How do I ask questions or suggest corrections? Any of these will do:
    - Leave a Disqus comment in the related page.
    - [Open an issue on GitHub](https://github.com/edom/edom.github.io/issues).
- How do you use a computer?
    - [Internet search tools]({% link search.md %})
    - [Remove nag screens]({% link nag.md %})
- [Long-haired men]({% link longhair.md %})
- Habits learned the hard way
    - I check the time at [time.gov](https://time.gov/) because Ubuntu 14.04 NTP once betrayed me.
        - 2018-01-16: I had always thought that it was dependable, but it betrayed me:
        It showed 2 a.m. while the actual time was 4 a.m..
        I missed some hours of sleep.
- Vital service failure log
    - Internet service providers
        - [Fastnet]({% link fastnet.md %})
        - [CBN]({% link cbn.md %})
    - Electricity distribution
        - [PLN]({% link pln.md %})
- [About this website]({% link about.md %})

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
