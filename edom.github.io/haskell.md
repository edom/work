---
title: Using Haskell
permalink: /haskell.html
date: 2018-05-16 00:00 +0700
language: en
---

- Setting up development environment
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
    - (2018-08-07) Download GHC 7.10 using Stack.
        - Which version of GHC should I use?
        You should use 7.10.
            - The one that is supported by [HaRe](http://hackage.haskell.org/package/HaRe) (Haskell refactoring tool) and other tools (IDE, Leksah, Cabal, etc.).
            In 2018, I think the safe choice is GHC 7.10.
        - The widely supported GHC version lags very much behind the latest stable GHC version.
        I think this may be because the GHC team is rolling out lots of breaking changes in the parser because they are working on the "Trees that grow" proposal.
    - unread, Stephen Diehl
        - http://www.stephendiehl.com/posts/vim_2016.html
        - http://www.stephendiehl.com/posts/vim_haskell.html
        - http://www.stephendiehl.com/posts/haskell_2018.html
- Haskell in 2018
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
- Haskell woes?
    - Exceptions?
        - http://hackage.haskell.org/package/safe-exceptions
        - https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell
        - https://www.reddit.com/r/haskell/comments/589fkg/haskell_and_the_no_runtime_exception_claim_95_of/
    - Module system
    - `Read(read)` should be renamed to `CoShow(coshow)`.
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
- Companies using Haskell
    - https://www.reddit.com/r/haskell/comments/4jo2da/fp_shops/
- things hot in 2018
    - [Cardano](https://cardanofoundation.org/)
        - https://www.reddit.com/r/haskell/comments/73r861/cardano_next_generation_blockchain_platform/
        - https://www.reddit.com/r/cardano/comments/8d87hf/haskell_cryptocurrencies/
    - IOHK
- Hackage outages
    - Sometimes Hackage goes down.
    - How to tell Cabal to use a Hackage mirror?
    An instruction is on the Internet; I forgot where.
    - 2018-04-13: [Hackage goes down for about a day](https://blog.hackage.haskell.org/posts/2018-04-26-downtime.html)
- Unread
    - https://github.com/dhall-lang/dhall-lang
    - https://wiki.haskell.org/Haskell_Communities_and_Activities_Report
    - https://haskellweekly.news/
        - https://github.com/haskellweekly/haskellweekly.github.io
            - https://wiki.haskell.org/Haskell_Weekly_News
    - https://haskell.libhunt.com/newsletter/6
- Cabal description field pitfall
    - http://michael.orlitzky.com/articles/using_haddock_markup_in_a_cabal_file.xhtml
- ghci
    - https://www.reddit.com/r/haskell/comments/5su9ag/reload_run_expressions_in_ghci_with_a_single/
- https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/
- https://stackoverflow.com/questions/15662984/speed-up-compilation-in-ghc?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa
- https://www.reddit.com/r/haskell/comments/45q90s/is_anything_being_done_to_remedy_the_soul/
- [Haddock markup syntax](https://www.haskell.org/haddock/doc/html/ch03s08.html)
- metaprogramming from Haskell to Haskell
    - [Strathclyde Haskell Enhancement](https://personal.cis.strath.ac.uk/conor.mcbride/pub/she/)
    - Template Haskell
    - https://wiki.haskell.org/Generics
        - SYB (Scrap Your Boilerplate), uniplate, etc.
- What does "Avoid 'success at all costs'" mean?
    - https://news.ycombinator.com/item?id=12056169
- The alternative to Cabal PVP is compile error, or, even worse, logic error and runtime failure?
    - Cabal PVP depends on library authors/maintainers to test and update their dependency bounds.
    - It's not only Haskell, but all programming.
    Every library author must thoughtfully maintain backward compatibility and can't just make arbitrary changes and break things.
        - https://plan99.net/~mike/writing-shared-libraries.html
- Will Eta kill [Frege](https://github.com/Frege/frege)?
It's sad to see works thrown away.
