# Erik Dominikus's works

These things are far from being finished.
I merge everything into one Git repository to simplify my workflow.

I share my goals and knowledge in the website [edom.github.io](https://edom.github.io/).

## Things maintained

- [org](org/): org-mode notes; should be exported into edom.github.io
- [etc](etc/): operating system configuration files
- [sh](sh/): shell scripts, mostly Bash
- [software](software/): a SWI-Prolog 7 knowledge base

## Things far from being finished; sometimes even abandoned; mostly doodles

- Haskell libraries
    - [meta](meta/)
        - library for writing metaprograms in Haskell
        - use Haskell as metalanguage
    - [prelude](prelude/): a maximalist Haskell Prelude that tries to collects best practices
    - [sound](sound/)
        - Find out whether real-time sound synthesis is possible with GHC Haskell.
        - Write some ugly code involving GHC primitives (unboxed things).
        - Hide the ugliness behind a newtype hidden in a module.
        - Assume that GHC inlines well.
    - [trade](trade/): Trade stocks in Indonesia
    - [haji](haji/) aspires to be a JVM written in Haskell but I lost my motivation very soon after I started.
    - [dynasty](dynasty/): doodling about something like Crusader Kings 2; trying to model the data
    - [dns-server](dns-server/)
    - [database](database/)
    - [pragmatic](pragmatic/)
    - [gen-site](gen-site/)
    - [ptt](ptt/): should be merged into `meta`
- Java libraries
    - [web](web/): various Java libraries; should be reorganized
    - [java-fun](java-fun/) tries to make Java more pleasant.
    - [control](control/)
- [devops](devops/): unclear, DevOps, infrastructure, cloud
- [research](research/): old books/writings to be reassembled into my website
- [archive](archive/): abandoned software

## Travis CI woes

- 2018-07-16: I stopped using Travis CI because `cabal new-build` ran out of memory.
- The `travis` branch
    - Pushing to this branch will trigger a rebuild.
    - This branch is meant to be rebased onto `origin/master`.
    - This branch is meant to be force-pushed.
    - The file `.travis.yml` shall stay in this branch.
    - Travis CI woes
        - @cabal new-build@ runs out of memory.
            - Reduce @-j4@ to @-j1@?
            - Add @optimization: False@ into @travis/cabal.config@.
                - @cabal --config-file=travis/cabal.config@
        - Related GHC woes
            - [#9221 (super!) linear slowdown of parallel builds on 40 core machine](https://ghc.haskell.org/trac/ghc/ticket/9221)
- Some notes about @optional-packages@ in @cabal.project@:
    - Clone [Yi editor source](https://github.com/yi-editor/yi) into @yi@.
        - Hackage's yi-0.17.1 (Nov 2017) is not ready for GHC 8.

## Building the website edom.github.io

(This note is for myself.)

The source code is in [edom.github.io](edom.github.io/).

To serve the website locally, run the script [sh/jekserve](sh/jekserve).

To publish the website, run the script [sh/pub](sh/pub).

- Install Jekyll 3 on Ubuntu 14.04
    - Install the version in [GitHub pages dependency versions](https://pages.github.com/versions/).
    - Follow [Setting up your GitHub Pages site locally with Jekyll - User Documentation](https://help.github.com/articles/setting-up-your-github-pages-site-locally-with-jekyll/).
    - 2018-08-21
        - Install ruby 2.5.1.
        - `gem install bundler`
        - `bundle install`
        - `BUNDLE_GEMFILE=... bundle exec`
    - See (but don't blindly follow) https://gist.github.com/Piyush3dB/b7daa3f178746c7d7479ca1cbd694160
        - I changed my mind.
        Don't use rbenv.
        Just compile ruby from source or download a binary package for your operating system and put it somewhere in `~/.local`.
            - Use [rbenv](https://github.com/rbenv/rbenv) to install newer Ruby versions.
                - Ubuntu 14.04 Jekyll is too ancient for GitHub Pages.
                - [What are the differences between rbenv, rvm, and chruby?](https://stackoverflow.com/questions/22153521/what-are-the-differences-between-rbenv-rvm-and-chruby)
                    - We don't want to program in Ruby.
                    We just want to install Jekyll.
- How do we speed up Jekyll?
    - Is 6 seconds for 160 pages slow?
    - What is making it slow?
        - How do we find that out?
            - Is there a stack-sampling profiler for Ruby?
                - Yes, `conscho` in [SO 4092641](https://stackoverflow.com/questions/4092641/profiling-ruby-code) recommends `stackprof`.
    - Upgrading Ruby 1.9.1 to Ruby 2.5.1 speeds up Jekyll somewhat.
