- Things far from being finished
    - [meta](meta/)
        - library for writing metaprograms in Haskell
        - use Haskell as metalanguage
    - [sound](sound/)
        - Find out whether real-time sound synthesis is possible with GHC Haskell.
        - Write some ugly code involving GHC primitives (unboxed things).
        - Hide the ugliness behind a newtype hidden in a module.
        - Assume that GHC inlines well.
    - [site](site/)
- Knowledge base
    - Haskell
        - [Cabal tragedies](cabal.md)
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
- ramble
    - https://www.researchgate.net/project/Ontology-oriented-programming
    - http://www.doc.ic.ac.uk/~klc/OntProg.html
- https://stackoverflow.com/questions/3416980/why-arent-whole-program-optimizations-more-prevalent-now/27757382
- https://en.wikipedia.org/wiki/Multi-adjoint_logic_programming
- https://medium.com/netflix-techblog/linux-performance-analysis-in-60-000-milliseconds-accc10403c55
- Keynote - What's Different In Dotty by Martin Odersky https://www.youtube.com/watch?v=9lWrt6H6UdE
- Building the website
    - Install Jekyll 3 on Ubuntu 14.04
        - See (but don't blindly follow) https://gist.github.com/Piyush3dB/b7daa3f178746c7d7479ca1cbd694160
        - Use [rbenv](https://github.com/rbenv/rbenv) to install newer Ruby versions.
        - Ubuntu 14.04 Jekyll is too ancient for GitHub Pages.
    - Why is Jekyll slow on my machine?
        - It's fast on GitHub.
        - What is making it slow?
            - How do we find that out?
                - Is there a stack-sampling profiler for Ruby?
                    - Yes, `conscho` in [SO 4092641](https://stackoverflow.com/questions/4092641/profiling-ruby-code) recommends `stackprof`.
        - On second thought, it's not too slow.
