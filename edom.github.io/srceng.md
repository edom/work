---
title: Making a search engine
permalink: /srceng.html
date: 2018-08-11 01:30 +0700
---

- Stop words (word ignore list) deal with index construction, not about query comprehension.
The search may ignore stop words when crawling, but it should not ignore stop words in queries.
    - [WP:Stop words](https://en.wikipedia.org/wiki/Stop_words)
    - query comprehension
        - Every word changes the intention/meaning of the query.
        - We need contextual search engine.
- Google: 1998, article, "The Anatomy of a Large-Scale Hypertextual Web Search Engine", Sergey Brin, Lawrence Page, [pdf](http://ilpubs.stanford.edu:8090/361/1/1998-8.pdf)
- https://en.wikipedia.org/wiki/Search_engine_indexing
- 2018, early access book draft, "Deep learning for search", [paywall](https://www.manning.com/books/deep-learning-for-search)
    - I guess that one who masters this book should be able to build a Google.
- [The theory behind Apache Lucene](https://wiki.apache.org/lucene-java/InformationRetrieval)
- What is Apache Lucene?
    - 2018-08-12
        - "Apache Lucene is a high-performance, full-featured text search engine library." ([source](http://lucene.apache.org/core/7_4_0/core/overview-summary.html#overview.description))
        - "Lucene is a text search engine API. Specifically, Lucene is the guts of a search engine - the hard stuff.
        You write the easy stuff, the UI and the process of selecting and parsing your data files to pump them into the search engine, yourself."
        ([source](https://wiki.apache.org/lucene-java/FrontPage?action=show&redirect=FrontPageEN))
- How did people search libraries before computers were invented?
    - https://en.wikipedia.org/wiki/Concordance_(publishing)
    - https://en.wikipedia.org/wiki/Index_(publishing)
    - https://en.wikipedia.org/wiki/Library_catalog
- Is there a distributed search engine, something as good as Google, but not owned by a company?
    - https://en.wikipedia.org/wiki/Distributed_search_engine
    - https://fourweekmba.com/distributed-search-engines-vs-google/
    - 2011, article, "‘Sciencenet’—towards a global search and share engine for all scientific knowledge", [html](https://academic.oup.com/bioinformatics/article/27/12/1734/255451)
    - 2004, article, "Web search engine based on DNS", [arxiv](https://arxiv.org/abs/cs/0405099)
        - hierarchical distributed search engine
    - 2013, patent, "Pervasive search architecture", [patent](https://patents.google.com/patent/US20180181603A1/en)
    - YaCy, [homepage](https://yacy.net/en/index.html), [GitHub](https://github.com/yacy/yacy_search_server), [WP](https://en.wikipedia.org/wiki/YaCy)
        - 2014, article, "Description of the YaCy Distributed Web Search Engine", [pdf](https://www.esat.kuleuven.be/cosic/publications/article-2459.pdf)
            - What operation does the RWI (reverse word index) speed up?
                - RWI is a hash table that maps a word to a URL.
                An entry (k,v) in the RWI means that the word k is found in the document at the URL v.
                - What is the relationship between RWI and inverted index?
                    - An RWI is an inverted index.
                    - [WP:Inverted index](https://en.wikipedia.org/wiki/Inverted_index)
        - YaCy doesn't have DHT (distributed hash table) routing.
        What does that mean?
        Why would one want DHT routing?
        Why would DHT imply routing?
            - 2017, Michael Dufel, "Because DHT nodes don’t store all the data, there needs to be a routing layer so that any node can locate the node that stores a particular key."
            ([source](https://medium.com/@michael.dufel_10220/distributed-hash-tables-and-why-they-are-better-than-blockchain-for-exchanging-health-records-d469534cc2a5))
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
            It's cheap (USD 4.28 per month in us-west-1/us-central-1/us-east-1? Really? What about network data transfer costs into?).
            It might be even cheaper (GCP free tier).
                - Even cheaper: Run YaCy on GCE preemptible instances?
                - alternative: DigitalOcean? https://www.digitalocean.com/pricing/
            - https://www.reddit.com/r/privacy/comments/1gbtlf/can_someone_please_explain_how_yacy_and_seeks/
        - Problems
            - YaCy, Solr, who knows what, sometimes runs out of memory.
            How come?
            - Does YaCy deal with stemming and synonyms?
            For example, compare the results for "using media in politics", "use media in politics", and "usage of media in politics".
        - How does YaCy use Solr?
            - http://www.yacy-websearch.net/wiki/index.php/Dev:Solr
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
        - How do Lucene, Solr, and Elasticsearch compare?
            - 2016, [quora](https://www.quora.com/How-do-Lucene-Elasticsearch-and-Solr-compare)
            - 2013, [SO 15704644](https://stackoverflow.com/questions/15704644/difference-between-solr-and-lucene)
            - https://logz.io/blog/solr-vs-elasticsearch/
            - http://solr-vs-elasticsearch.com/
            - http://lucene.apache.org/solr/
        - http://www.yacy-websearch.net/wiki/index.php/En:Features
        - http://www.yacy-websearch.net/wiki/index.php/En:Use_cases
        - How does Lucene work?
        - How does Solr work?
        - https://en.bitcoinwiki.org/wiki/YaCy
        - What is a DHT (distributed hash table)?
        How does it work?
        - Operating YaCy
            - Crawling
                - Regular expressions
                    - For syntax, see [java.util.regex.Pattern javadoc](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html).
                    - http://localhost:8090/RegexTest.html
                        - Match all non-English Wikipedia URL paths: `(?!en)...wikipedia.*`
                    - https://stackoverflow.com/questions/5319840/greedy-vs-reluctant-vs-possessive-quantifiers
                    - https://www.regular-expressions.info/lookaround.html
                    - https://www.regular-expressions.info/refadv.html
- https://www.quora.com/What-approaches-do-state-of-the-art-search-engines-use-for-stemming
- 2013, article, "The Next-Generation Search Engine: Challenges and Key Technologies", [paywall](https://link.springer.com/chapter/10.1007/978-3-642-28807-4_34)
- dead search engines
    - https://en.wikipedia.org/wiki/Seeks
    - succumbed to money
        - https://en.wikipedia.org/wiki/Blekko
- 1999 article [Indexing the Internet](http://www.tk421.net/essays/babel.html)
