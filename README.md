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
- The `travis` branch
    - Pushing to this branch will trigger a rebuild.
    - This branch is meant to be rebased onto `origin/master`.
    - This branch is meant to be force-pushed.
    - The file `.travis.yml` shall stay in this branch.
    - <a href="https://travis-ci.com/edom/work"><img alt="build status" src="https://travis-ci.com/edom/work.svg?branch=travis"></a>
    - Travis CI woes
        - @cabal new-build@ runs out of memory.
            - Reduce @-j4@ to @-j1@?
            - Add @optimization: False@ into @travis/cabal.config@.
                - @cabal --config-file=travis/cabal.config@
- Some notes about @optional-packages@ in @cabal.project@:
    - Clone [Yi editor source](https://github.com/yi-editor/yi) into @yi@.
        - Hackage's yi-0.17.1 (Nov 2017) is not ready for GHC 8.
