---
title: Web scraping
permalink: /scrape.html
date: 2018-05-15 02:31:00 +0700
---

- Web scraping with Haskell
    - Easily manipulate DOM using [scalpel](https://hackage.haskell.org/package/scalpel),
    which uses [tagsoup](https://hackage.haskell.org/package/tagsoup).
    - Consume and produce REST API using
    [a Haskell REST framework](http://engineering.silk.co/post/90354057868/announcing-rest-a-haskell-rest-framework).
    - [A rather low-level approach](https://www.schoolofhaskell.com/school/to-infinity-and-beyond/competition-winners/interfacing-with-restful-json-apis).
    - We need an HTTP REST API.
    There are many ways of passing parameters: query string, http header, multipart.
    Request body can contain bytes, or JSON.
    Needs jsonification and dejsonification.
    - [HXT](https://wiki.haskell.org/HXT): how is this package organized?
    - [Building a concurrent web scraper with Haskell](http://adit.io/posts/2012-03-10-building_a_concurrent_web_scraper_with_haskell.html)
