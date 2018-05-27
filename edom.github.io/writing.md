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
