---
title: Software
permalink: /software.html
date: 2017-06-26 22:30 +0700
---

- Web development
    - [HSLUV: human-friendly *perceptually uniform* color picker](http://www.hsluv.org/)
        - [Wikipedia: CIELUV color space](https://en.wikipedia.org/wiki/CIELUV)
        - [Good colour maps: how to design them](https://arxiv.org/abs/1509.03700), 2015, Peter Kovesi
    - [Lea Verou's contrast ratio checker tool](https://leaverou.github.io/contrast-ratio/)
    - [Set up Webpack to transpile and bundle TypeScript sources](https://webpack.js.org/guides/typescript/)
    - HTML DOM, web, browser, JavaScript
        - [What forces layout / reflow](https://gist.github.com/paulirish/5d52fb081b3570c81e3a)
- Software for scientists
    - [stared github gist](https://gist.github.com/stared/9130888)
    - https://www.quora.com/What-wiki-blog-software-do-PhD-students-use-to-maintain-personal-notes-of-their-daily-reading-research
- Legality of software
    - Why doesn't Facebook just keep using Apache Software License version 2.0 for React?
    Why does it roll out its own patent license?
        - Update: Facebook has switched back to Apache-2.0.
- [Autotools]({% link autotools.md %})
- Functional programming
    - Haskell
        - https://github.com/sellout/recursion-scheme-talk/blob/master/recursion-scheme-talk.org
        - https://github.com/krispo/awesome-haskell
        - Cabal is the key to Haskell usability and adoption?
            - https://www.haskell.org/cabal/
            - Haskell lacks something like ruby gem, python pypi, or nodejs npm.
                - https://stackoverflow.com/questions/5138881/how-are-ghc-pkg-and-cabal-programs-related-haskell
                - https://stackoverflow.com/questions/2706667/what-is-the-relationship-between-ghc-pkg-and-cabal
    - Pure-lang
        - https://puredocs.bitbucket.io/pure.html#lazy-evaluation-and-streams
        - https://bitbucket.org/purelang/pure-lang/wiki/Rewriting
        - https://agraef.github.io/pure-docs/#language-and-standard-library
        - https://agraef.github.io/pure-docs/pure.html#pure-overview
        - https://wiki.haskell.org/Applications_and_libraries/Music_and_sound
- Ungrouped
    - [doppio](https://github.com/plasma-umass/doppio), a JVM written in TypeScript, with a POSIX-compatible runtime system
        - from https://www.reddit.com/r/programming/comments/3xkn75/nashorn_javascript_on_the_jvm_ftw/
- My abandoned software
    - These are not usable.
    - [Pragmatic Haskell library](https://github.com/edom/pragmatic)
    tries to standardize the ways of doing things.
    - [Try Phabricator](https://github.com/edom/try-phabricator)
    uses Docker Compose and is bundled with Apache, PHP, and MariaDB.
    It worked out of the box, but it was not designed for production.
    - [APT manual mirror](https://github.com/edom/apt-manual-mirror) copies selected Debian packages
    into local directory while preserving the layout.
    It allows you to mirror only the packages you want.
    - [Haji](https://github.com/edom/haji) tries to be a Java bytecode interpreter written in Haskell.
