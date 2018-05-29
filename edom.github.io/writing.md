---
title: Writing
permalink: /writing.html
date: 2017-05-20 21:29:00 +0700
---

- Sections
    - [Writing for screen]({% link writscr.md %})
    - [Gender-neutral singular third-person pronoun]({% link pronoun.md %})
- Links
    - [WP:Content engineering](https://en.wikipedia.org/wiki/Content_Engineering)
- Books should be shorter.
    - I think a 100-page popular book has only 1 page of useful information.
    - [WP:Inverted pyramid](https://en.wikipedia.org/wiki/Inverted_pyramid_(journalism))
        - Begin with the most important statement.
        - The rest of the book supports that statement.
- Undigested information
    - [YC:The sad state of personal knowledgebases](https://news.ycombinator.com/item?id=10739227)
    - [Quora: Which is better, Markdown or Textile?](https://www.quora.com/Which-is-better-Markdown-or-Textile?share=1)

- Why do I write?
    - **To connect my future self and my past self**.
    Sometimes my past self makes a decision that my future self regrets.
    When that happens, my future self wants to trace back
    how my past self could have made such decision.
    - **To change the world to what I want it to be**.
    I think writing is the most efficient way of
    spreading information and influencing people.
    Telepathic broadcasting would be more efficient,
    but I don't know how to do that.
    Anyway, it's up to the world
    whether they want to change.
    - **To save your time**.
    I'm happy when my writing helps you understand something faster.
- Architecture
    - Each document in this site is a living document.
    - Decision: a website, not a book.
        - Reason: To minimize the readers' effort.
- Rules
    - As long as this site is hosted on GitHub,
    all contents in this site shall follow the
    [GitHub Terms of Service](https://help.github.com/articles/github-terms-of-service/).
    - Ads are OK as long as they don't hamper the reading experience.
    They must not slow down the page.
    They must not cover the content.
    - Pop-ups are not OK.
- The technology decisions for this wiki and why:
    - Use technology that should still work at least until year 2100.
        - Use only free open-source technologies with proven maintenance track record.
        - Enable rebuilding of the website somewhere else quickly.
    - Use Jekyll-powered GitHub Pages. If GitHub screws us, we buy a new domain, move to AWS, and ask Google to reindex.
    - Use GitHub Flavored Markdown. If Jekyll screws us, we switch to Pandoc.
    - Markdown weaknesses:
        - The original Markdown doesn't have tables.
        - Proliferation of incompatible dialects.
    - Wiki vs blog, page vs post
        - A page is timeless. Its usefulness should not depend on time.
        Old pages are still useful.
        - A post is time-sensitive. Its usefulness depends on time.
        Old posts are useless.
        - Posts are good for news, updates, changelogs, diffs.
    - Alternatives?
        - http://community.schemewiki.org/
        - http://www.nongnu.org/skribilo/
        - Skribilo is a free document production tool that takes a structured document representation as its input
        and renders that document in a variety of output formats: HTML and Info for on-line browsing, and Lout and
        LaTeX for high-quality hard copies.
- 2017-05-18
    - What to write?
        - Something you want to write.
        - Something a group of people want to read.
- 2017-05-18
    - Installing Jekyll on Ubuntu 14
        - Ubuntu 14 has Ruby 1.9.1 package and Jekyll 0.11.2.
        - Jekyll 3.4.3 requires Ruby 2.
        - [Download Ruby 2.4.1](https://www.ruby-lang.org/en/downloads/).
        - Run:
```
sudo apt-get install libssl-dev bison
cd ruby-2.4.1
configure --prefix=$HOME/.local
make -j4 install
```
- Information visualization
    - [WP:Exploration](https://en.wikipedia.org/wiki/Exploration)
    - [WP:Exploration problem](https://en.wikipedia.org/wiki/Exploration_problem)
    - [WP:Information visualization](https://en.wikipedia.org/wiki/Information_visualization)
    - Example of information visualization?
        - https://www.gartner.com/technology/research/digital-marketing/transit-map.jsp
- 2016-04-16T19:32:18Z
    - What should we write?
        - Answers from pragmaticism and capitalism:
            - Write something that our intended audience find *useful*.
            - Treat our writing as *asset*.
            - Ask ourselves: what problem are we solving?
        - We can write anything you want to,
        but we will be a better writer
        if we understand *why people read*.
        - *People read to solve their problems.*
            - Some need to fix a computer; help articles fix their problem.
            - Some need to decide if they will invest in a company; reports fix their problem.
        - We want to write something good. What is a good writing?
            - Let me use *pragmaticism* and *capitalism* to define a good writing;
            you can later define what is good for you
            according to whatever moral system you subscribe to.
                - Pragmaticism suggests that a good writing is useful
                in the sense that the writing *solves* the problem of its intended readers.
                - Capitalism suggests that a good writing is
                a *capital*, an asset, a means of production.
                For example, some novels give rise to films, games, and merchandises;
                such writings are great assets that will bring wealth to their owners.
                A writing published on the Internet is also an asset for generating traffic and trust,
                which can sometimes then translate into money through advertising
                (or whatever you can convert others' trust to).
