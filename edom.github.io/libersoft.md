---
title: Liberating software
date: 2018-08-07 20:23 +0700
permalink: /libersoft.html
---

- Is there a distributed search engine, something as good as Google, but not owned by a company?
    - https://en.wikipedia.org/wiki/Distributed_search_engine
    - https://fourweekmba.com/distributed-search-engines-vs-google/
    - YaCy, [homepage](https://yacy.net/en/index.html), [GitHub](https://github.com/yacy/yacy_search_server), [WP](https://en.wikipedia.org/wiki/YaCy)
        - [2011 article, LWN](https://lwn.net/Articles/469972/)
        - [2011 article, pcmag review](https://www.pcmag.com/article2/0,2817,2397267,00.asp)
        - Setting up YaCy
            - Clone YaCy GitHub
            - Prepare isolation
                - `sudo adduser --system --group yacy`
                - `sudo adduser $SUDO_USER yacy`, replace `$SUDO_USER` with your non-yacy user
                - `sudo chown -R yacy:yacy`
            - Build YaCy
                - `sudo -u yacy -H /bin/bash` for shell
                - In that shell:
                    - `ant clean all`
            - Start YaCy
                - Still in the yacy shell:
                    - `./startYACY.sh -f` to start YaCy in foreground
            - Use YaCy
                - Open [http://localhost:8090/](http://localhost:8090/)
            - Can we make it easier to setup YaCy peer network?
                - My scenario is typical:
                    - I'm behind two layers of NAT: my ISP's router, and my USB-tethered Android phone.
                    - Overlay network? VPN? Hosting on cloud?
            - Should we host YaCy on GCP (Google Cloud Platform)?
            It's cheap (USD 3.88 per month in us-west-1/us-central-1/us-east-1? Really? What about network data transfer costs into?).
            It might be even cheaper (GCP free tier).
                - alternative: DigitalOcean? https://www.digitalocean.com/pricing/
            - https://www.reddit.com/r/privacy/comments/1gbtlf/can_someone_please_explain_how_yacy_and_seeks/
        - If YaCy can have an overlay network (if public YaCy nodes can function as forwarders), it can have more nodes.
        - https://en.wikipedia.org/wiki/Dooble
        - https://yacy.net/en/index.html
            - How do I use it?
            - What can I use it for?
        - How many people use YaCy?
        - There is [online demo](https://yacy.net/en/Searchportal.html).
        It may fail.
        - What are YaCy alternatives?
        It is the most widely used?
        What is the future?
        How many developers?
        - Are there text (non-video) tutorials?
        There is a [wiki](http://www.yacy-websearch.net/wiki/index.php/En:Start).
        - How does YaCy handle adversarial/malicious peers?
            - https://yacy.net/en/Technology.html
        - How do you make YaCy your personal search engine?
        How do you tailor YaCy to your needs?
        - Is YaCy the state of the art?
    - What are alternatives to Google?
    Are these real alternatives?
    Which are usable?
        - https://restoreprivacy.com/google-alternatives/
        - https://degooglisons-internet.org/en/
            - https://news.ycombinator.com/item?id=13140389
    - https://www.quora.com/What-approaches-do-state-of-the-art-search-engines-use-for-stemming
    - 2013, article, "The Next-Generation Search Engine: Challenges and Key Technologies", [paywall](https://link.springer.com/chapter/10.1007/978-3-642-28807-4_34)
    - dead
        - https://en.wikipedia.org/wiki/Seeks
        - succumbed to money
            - https://en.wikipedia.org/wiki/Blekko
- 2016, article, "The Fathers of the Internet Revolution Urge Today’s Software Engineers to Reinvent the Web", Tekla S. Perry, [html](https://spectrum.ieee.org/view-from-the-valley/telecom/internet/the-fathers-of-the-internet-revolution-urge-todays-pioneers-to-reinvent-the-web).
See the bulleted lists.
