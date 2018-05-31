---
title: About
date: 2017-05-25 01:00:00 +0700
permalink: /about.html
---

- [Similar websites]({% link similar.md %})
- [All pages]({% link all.md %})
- 2017-05-25:
Thanks to [David Ensinger](http://davidensinger.com/)'s
[code](http://davidensinger.com/2013/11/building-a-better-sitemap-xml-with-jekyll/),
Jekyll can generate sitemap.xml without plugins.
    - Problem: can't infer last modified date from Git repository.
        - This affects Google crawl?
            - Yes.
                - https://www.sitemaps.org/protocol.html
                    - sitemap.xml lastmod is optional but
                    "This incremental Sitemap fetching mechanism allows for the rapid discovery of new URLs on very large sites."
            - No.
                - https://webmasters.stackexchange.com/questions/25833/does-google-penalize-daily-updated-lastmod-tags-in-sitemaps-if-the-data-is-not
                    - "The lastmod tag is optional in sitmaps and in most of the cases it's ignored by search engines,
                    because webmasters are doing a horrible job keeping it accurate."
        - Solution candidates:
            - Manually update lastmod every time we edit an md file
            - Write a script to update lastmod in each md file
                - all files, or only those in a commit?
                - https://serverfault.com/questions/401437/how-to-retrieve-the-last-modification-date-of-all-files-in-a-git-repository
                - https://stackoverflow.com/questions/14141344/git-status-list-last-modified-date
                - https://hackerific.net/2016/04/30/git-file-age-a-script-to-show-when-files-were-last-modified-in-git/
            - Stay in Jekyll, modify sitemap generator
                - https://github.com/gjtorikian/jekyll-last-modified-at
                    - not compatible with github pages https://github.com/gjtorikian/jekyll-last-modified-at/issues/32
                - https://stackoverflow.com/questions/14978474/how-to-show-the-modification-date-of-a-file-in-jekyll
                - https://milanaryal.com.np/jekyll-site-last-modified-time/
            - Generate sitemap.xml outside Jekyll
            - Run Jekyll locally, commit output html to github
- Why is the font so big?
    - Because I'm myopic.
    - To shrink the text, press Control-minus.
