---
title: Using Haskell
permalink: /haskell.html
date: 2018-05-16 00:00 +0700
language: en
---

- TOC
{:toc}

## Setting up development environment

- Duplicate intersecting efforts?
Too many choices?
    - Why is there Haskell Platform and Haskell Stack?
    - Which should we use?
    - Why is there haskell-lang.org and haskell.org?
        - https://news.ycombinator.com/item?id=12054690
    - Why should I use Platform if there is Stack?
    - Why should I use Stack if there is Cabal new-style?
    - Why Stack?
        - Vetted packages?
    - http://www.haskellforall.com/2018/05/how-i-evaluate-haskell-packages.html
- cabal new-build obviates stack?
    - http://coldwa.st/e/blog/2017-09-09-Cabal-2-0.html

### My incoherent rambling

My old way: stack.
My new way: cabal new-style.
It may change again.

- Install the Haskell `stack` tool.
    - I use the [manual download](https://docs.haskellstack.org/en/stable/install_and_upgrade/#linux).
        - If you want to make sure that the download isn't corrupted, check the corresponding sha256sum from [GitHub releases page](https://github.com/commercialhaskell/stack/releases/).
        - If you don't mind sudoing, use the installer script in the [documentation](https://docs.haskellstack.org/en/stable/README/).
    - Then I check the archive contents using `tar tzf`.
    - If there's no weird paths, I extract the archive with `tar xzf`.
    - Then I make the symbolic link `~/.local/bin/stack`.
    - If you use the manual download, you may have to install some operating system packages.
        - The list is on the [install script](https://get.haskellstack.org/).
        Search for `install_dependencies` for your distro.
- Choose a Stack solver.
    - Forget it. Just install GHC to home.
        - `./configure --prefix ~/.local`
        - `make -j4 install`
- Which version of GHC should I use?
        - The one that is supported by
        [HaRe](http://hackage.haskell.org/package/HaRe) (Haskell refactoring tool) and
        [Leksah](https://github.com/leksah/leksah).
            - On 2018-08-20, this is 8.0.2.
                - Leksah requires ghc >= 8.0.2.
                - HaRe supports ghc <= 8.0.2.
                - GHC 8.0 is unacceptably slow.
                    - Forget HaRe. We'll go with Leksah. Use GHC 8.4.3.
    - The widely supported GHC version lags very much behind the latest stable GHC version.
    I think this may be because the GHC team is rolling out lots of breaking changes in the parser because they are working on the "Trees that grow" proposal.
    - Which Stackage LTS version should I use?
        - [LTS 6.35](https://www.stackage.org/lts-6.35) if GHC 7.10.3?
            - It also hosts a hoogle search for searching Haskell program elements.
- How to get started?
    - Too many choices
        - install using the package manager that comes with your system
            - pros: least hassle
            - cons: outdated software
        - Stack
        - Cabal
        - Nix
        - Haskell Platform
- What is your preferred way of installing Haskell?
    - Install `cabal-install`
        - Download the suitable `cabal-install` binary package from https://www.haskell.org/cabal/download.html
        - Extract the `cabal` binary to `~/.local/bin`
    - Install current stable release of GHC
        - Download the current stable release of GHC from https://www.haskell.org/ghc/download.html
        - Extract it somewhere
        - Follow the instructions in INSTALL file:
            - `./configure --prefix=$HOME/.local`
            - `make -j4 install`
    - Modify `PATH` in `~/.basrhc`:
        - Ensure that the line `export PATH="$PATH:$HOME/.local/bin"` is in `~/.bashrc`.
    - For what is the hassle?
        - So that, if anything goes wrong, I can nuke it without nuking my whole operating system.

## Hackage outages

Hackage is Haskell package repository.
Sometimes it goes down.

- How to tell Cabal to use a Hackage mirror?
An instruction is on the Internet; I forgot where.
- 2018-04-13: [Hackage goes down for about a day](https://blog.hackage.haskell.org/posts/2018-04-26-downtime.html)

## Haskell in 2018

- unread, Stephen Diehl
    - http://www.stephendiehl.com/posts/vim_2016.html
    - http://www.stephendiehl.com/posts/vim_haskell.html
    - http://www.stephendiehl.com/posts/haskell_2018.html
    - https://www.reddit.com/r/haskell/comments/7wmhyi/an_opinionated_guide_to_haskell_in_2018/
    - https://github.com/Gabriel439/post-rfc/blob/master/sotu.md
    - https://www.reddit.com/r/haskell/comments/54fv8b/what_is_the_state_of_haskell/
    - https://lexi-lambda.github.io/blog/2018/02/10/an-opinionated-guide-to-haskell-in-2018/
    - [Eta: Haskell on JVM](https://eta-lang.org/)
- Development workflow and tools
    - 2018
        - IDE (integrated development environment)
            - Visual Studio Code
            - Leksah
            - others?
            - https://github.com/haskell/haskell-ide-engine
            - wasted efforts?
                - [EclipseFP](https://github.com/JPMoresmau/eclipsefp), no longer developed since 2015
                    - http://jpmoresmau.blogspot.com/2015/05/eclipsefp-end-of-life-from-me-at-least.html
                        - He got tired of working alone.
                        He pointed us to [FPComplete ide-backend](https://www.fpcomplete.com/blog/2015/03/announce-ide-backend).
                            - [ide-backend](https://github.com/fpco/ide-backend) seems dead; last activity is in 2016.
        - cabal new-style
    - 2016, http://www.stephendiehl.com/posts/vim_2016.html
        - 2015 (?), http://www.stephendiehl.com/posts/vim_haskell.html
- Companies using Haskell
    - https://www.reddit.com/r/haskell/comments/4jo2da/fp_shops/
- things hot in 2018
    - [Cardano](https://cardanofoundation.org/)
        - https://www.reddit.com/r/haskell/comments/73r861/cardano_next_generation_blockchain_platform/
        - https://www.reddit.com/r/cardano/comments/8d87hf/haskell_cryptocurrencies/
    - IOHK
- Unread
    - https://github.com/dhall-lang/dhall-lang
    - https://wiki.haskell.org/Haskell_Communities_and_Activities_Report
    - https://haskellweekly.news/
        - https://github.com/haskellweekly/haskellweekly.github.io
            - https://wiki.haskell.org/Haskell_Weekly_News
    - https://haskell.libhunt.com/newsletter/6

## Using GHC

- Using GHCI
    - https://www.reddit.com/r/haskell/comments/5su9ag/reload_run_expressions_in_ghci_with_a_single/
- https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/
- https://stackoverflow.com/questions/15662984/speed-up-compilation-in-ghc?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
- https://www.reddit.com/r/haskell/comments/45q90s/is_anything_being_done_to_remedy_the_soul/

## what

- [Haddock markup syntax](https://www.haskell.org/haddock/doc/html/ch03s08.html)
- metaprogramming from Haskell to Haskell
    - [Strathclyde Haskell Enhancement](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/)
    - Template Haskell
    - https://wiki.haskell.org/Generics
        - SYB (Scrap Your Boilerplate), uniplate, etc.
- What does "Avoid 'success at all costs'" mean?
    - https://news.ycombinator.com/item?id=12056169
- Will Eta kill [Frege](https://github.com/Frege/frege)?
It's sad to see works thrown away.

## Finding a Haskell IDE

I haven't found a convincing IDE for Haskell.

- [what IDE/editor do you use for Haskell development? : haskell](https://www.reddit.com/r/haskell/comments/5lgtb1/what_ideeditor_do_you_use_for_haskell_development/)
- [What are powerful Haskell IDEs? - Quora](https://www.quora.com/What-are-powerful-Haskell-IDEs)
- [What is the best IDE for programming in Haskell? - Quora](https://www.quora.com/What-is-the-best-IDE-for-programming-in-Haskell)
- [Haskell ides? : haskell](https://www.reddit.com/r/haskell/comments/86bmpu/haskell_ides/)
- Leksah-nix fails to build on my machine (Ubuntu 14.04).
There are no prebuilt binaries.
Must compile from source from [Hackage](https://github.com/leksah/leksah/wiki/download) using Cabal.

## what

- Enterprise Haskell?
    - https://wiki.haskell.org/Enterprise_Haskell
    - DSH: Database Supported Haskell https://hackage.haskell.org/package/DSH
- [Distributed Systems in Haskell :: Will Yager](http://yager.io/Distributed/Distributed.html)
- Alien technologies?
    - https://github.com/transient-haskell/transient

## what

- useful trick, especially helpful when abusing type classes: https://chrisdone.com/posts/haskell-constraint-trick
- https://chrisdone.com/posts/twitter-problem-loeb
- http://blog.sigfpe.com/2006/11/from-l-theorem-to-spreadsheet.html
- http://blog.sigfpe.com/2007/02/comonads-and-reading-from-future.html
- Components for publish-subscribe in Haskell?
    - https://www.stackage.org/lts-6.35/package/broadcast-chan-0.1.1
- Lennart Augustsson's [Things that amuse me](http://augustss.blogspot.com/2008/12/somewhat-failed-adventure-in-haskell.html), Haskell module overloading
- [lazy: Explicit laziness for Haskell](http://hackage.haskell.org/package/lazy)
    - "This library provides laziness as an abstraction with an explicit type-signature, and it so happens that this abstraction forms a monad!"
    - [If Haskell were strict, what would the laziness be like?](https://nikita-volkov.github.io/if-haskell-were-strict/)
    - 2014, article, [Paul Chiusano: An interesting variation on a strict by default language](https://pchiusano.github.io/2014-09-18/explicit-laziness.html)
    - [How do we all feel about laziness? : haskell](https://www.reddit.com/r/haskell/comments/36s0ii/how_do_we_all_feel_about_laziness/)
- distributed functional programming?
    - [WP:MBrace](https://en.wikipedia.org/wiki/MBrace), F#
    - [Cloud Haskell](https://haskell-distributed.github.io/)
        - has some academic papers https://wiki.haskell.org/Cloud_Haskell
    - [PatrickMaier/HdpH: Haskell distributed parallel Haskell](https://github.com/PatrickMaier/HdpH)

## Curating libraries

- https://www.reddit.com/r/haskell/comments/4ggt05/best_underrated_haskell_libraries/
- https://wiki.haskell.org/Applications_and_libraries
- https://stackoverflow.com/questions/9286799/haskell-libraries-overview-and-their-quality

## what

- unread
    - servant web framework
    - Salsa Haskell .NET bridge
        - https://wiki.haskell.org/Salsa
    - [Haskell partiality monad](https://gist.github.com/puffnfresh/6222797)
- https://stackoverflow.com/questions/5770168/templating-packages-for-haskell
- Hoogle vs Hayoo?
    - The hoogle on stackage.org top right text bar seems to be most complete
        - https://www.stackage.org/
    - https://mail.haskell.org/pipermail/haskell-cafe/2013-August/109945.html

## Haskell woes

- Exceptions?
    - http://hackage.haskell.org/package/safe-exceptions
    - https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
    - https://www.reddit.com/r/haskell/comments/589fkg/haskell_and_the_no_runtime_exception_claim_95_of/
- Module system
- `Read(read)` should be renamed to `CoShow(coshow)`.

## GHC woes

- Profiling requires recompiling all transitive dependencies if they happen to be compiled without profiling.

### Working on GHC

- Beginning to work on GHC
    - Please see the [newcomers guide](https://ghc.haskell.org/trac/ghc/wiki/Newcomers) first.
- GHC TDNR (type-directed name resolution)
    - https://ghc.haskell.org/trac/ghc/ticket/4479
    - https://ghc.haskell.org/trac/ghc/wiki/Records/OverloadedRecordFields
    - https://stackoverflow.com/questions/22417063/current-state-of-record-types-and-subtyping-in-haskell
    - https://en.wikipedia.org/wiki/Subtyping#Record_types
        - Width and depth subtyping

## People who have too much time

- https://hackage.haskell.org/package/ImperativeHaskell
- just for curiosity https://github.com/edwinb/idris-php

## What's hampering Haskell adoption?

- GHC's aggressive intermodule optimization precludes prebuilt binaries.
