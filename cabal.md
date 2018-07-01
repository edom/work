# Cabal tragedies

- Every package used by Setup.hs must have a vanilla version.
    - Why I encountered this error:
        - I set `library-vanilla` to `False` due to https://rybczak.net/2016/03/26/how-to-reduce-compilation-times-of-haskell-projects/
    - How I encountered this error:
        - HDBC-postgresql-2.3.2.5 `Setup.hs` build fails due to linking error with `shared: True`.
            - The offending packages are `old-time` and `old-locale`.
                - It's OK if we build them as shared library, but we must also build their vanilla version.
            - It's not the library content that fails to build. It's the Cabal Setup of the library that fails to link.
            - GHC expects that `old-time` and `old-locale` are system libraries?
            - GHC passes '-lHSold-time-VERSION-HASH.so'. It passes that in the -l switch to GCC.
            - Cabal writes 'libHSold-time-VERSION-HASH-ghc-8.2.2.so'.
                - Note the `ghc-8.2.2` part isn't in the string passed by GHC to GCC.
            - Can we solve this by `cabal install old-time old-locale`?
            - https://github.com/haskell/cabal/issues/1720
                - Workaround: Add `--ghc-options=-dynamic` to cabal new-install
            - How do we tell Cabal to use the version we installed with new-install?
            - Where should we fix this? Cabal? GHC? HDBC-postgresql?
            - Should we find another library? Hackage has a low-level libpgsql wrapper.
            - Should we just disable HDBC-postgresql on meta?
    - How I diagnosed it:
        - Pass `-v` to GHC (create a bash script named `ghc` that calls `SOMEWHERE/ghc -v "$@"`, and put its directory in front of `PATH`).
        - Add `-v` to `cabal`. Then look at this fragment. It's suspicious that `old-time` is the only package with a hash.
```
package flags [-package-id Cabal-2.0.1.0{unit Cabal-2.0.1.0 True ([])},
           -package-id array-0.5.2.0{unit array-0.5.2.0 True ([])},
           -package-id base-4.10.1.0{unit base-4.10.1.0 True ([])},
           -package-id binary-0.8.5.1{unit binary-0.8.5.1 True ([])},
           -package-id bytestring-0.10.8.2{unit bytestring-0.10.8.2 True ([])},
           -package-id containers-0.5.10.2{unit containers-0.5.10.2 True ([])},
           -package-id deepseq-1.4.3.0{unit deepseq-1.4.3.0 True ([])},
           -package-id directory-1.3.0.2{unit directory-1.3.0.2 True ([])},
           -package-id filepath-1.4.1.2{unit filepath-1.4.1.2 True ([])},
           -package-id ghc-prim-0.5.1.1{unit ghc-prim-0.5.1.1 True ([])},
           -package-id old-time-1.1.0.3-8c2cc8e5fb3b424e71501141225064c5d9ee4eeba7f40b702227ad1c3ea2c5b7{unit old-time-1.1.0.3-8c2cc8e5fb3b424e71501141225064c5d9ee4eeba7f40b702227ad1c3ea2c5b7 True ([])},
           -package-id pretty-1.1.3.3{unit pretty-1.1.3.3 True ([])},
           -package-id process-1.6.1.0{unit process-1.6.1.0 True ([])},
           -package-id template-haskell-2.12.0.0{unit template-haskell-2.12.0.0 True ([])},
           -package-id time-1.8.0.2{unit time-1.8.0.2 True ([])},
           -package-id transformers-0.5.2.0{unit transformers-0.5.2.0 True ([])},
           -package-id unix-2.7.2.2{unit unix-2.7.2.2 True ([])}]
```
    - How I solved it:
        - I added this fragment to `cabal.project`:
```
package old-time
library-vanilla: True

package old-locale
library-vanilla: True
```
    - Related
        - https://github.com/haskell/cabal/issues/4748
        - [#3409 Can't use system GHC without static libraries at all](https://github.com/commercialhaskell/stack/issues/3409)
        - [#1720 `executable-dynamic: True` should apply to `build-type: Custom` setup](https://github.com/haskell/cabal/issues/1720)
- [#5290 new-build runs into internal error after deleting from store](https://github.com/haskell/cabal/issues/5290)
    - Solution: nuke the entire store: `rm -r ~/.cabal/store`.
- Non-problems
    - `cabal new-build --disable-optimization` doesn't disable optimization of transitive dependencies.
        - Cannot reproduce this in cabal 2.3. Is this a 2.0.0.1 bug?
        - `$HOME/.cabal/config` has `optimization: False`.
        - Is this a regression? Oversight? It works with `cabal install`.
        - What I'm trying to do:
            - Build transitive dependencies with optimization disabled, for faster development.
        - My guess:
            - There seems to be a problem in how the new code path plumbs down arguments.
            - `"Using internal setup method with build-type"` always gets argument `--enable-optimization`.
                - That message is printed by `./Distribution/Client/SetupWrapper.hs:418` `internalSetupMethod` if `cabal new-repl` is run with `--verbose`.
                - Where does that `--enable-optimization` come from?
                - Why isn't the `--disable-optimization` passed down?
        - Related issues:
            - [#3720 Tracking bug for cabal.project semantics](https://github.com/haskell/cabal/issues/3720)
            - [#5353 cabal new-configure --disable-optimization has no effect if ghc-options in cabal file contain optimization flag](https://github.com/haskell/cabal/issues/5353)
- Cabal codebase
    - Seemingly minor codebase maintenance problems
        - Code duplication
            - `CmdRepl.hs` seems to be copied from `CmdBuild.hs`.
        - `CmdBuild.hs` imports `...Orchestration` unqualified without explicit import list
