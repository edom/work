This package sometimes fails to build.
Removing hakyll version constraint sometimes solves the problem.

```
Resolving dependencies...
Error:
    Dependency on unbuildable library from pandoc
    In the stanza 'library'
    In the inplace package 'gen-site-0.0.0'
```

- Does that mean pandoc is to blame?
- Does that mean hakyll is to blame?
- 2018, Cabal programming error, https://github.com/haskell/cabal/issues/5325
    - Cabal fails to backtrack on `buildable: False`.

- Plans
    - Modernize this (it uses ghc 7, cabal 1, pandoc, hakyll).
    - Things that must be done anyway.
        - Fix broken markup.
            - Choose:
                - Find-and-replace with sed or something like that.
                - Reimplement the subset of Jekyll/Liquid that we use (`link`).
        - Treat GFM `\\(` as Pandoc-Markdown `\(` (begin inline math).
            - That's my biggest complaint about GFM.
            Pandoc translates `\(` to `\(` whereas GFM translates `\(` to `(`.
            GFM loses the backslash.
            Thus to use MathJax, I have to type `\\(` with GFM.
            With Pandoc I can just type `\(`.
        - Maintain compatibility with Jekyll?
    - Would be nice.
        - Generate proper sitemap.
    - Later, if ever.
        - distributed web hosting
            - https://en.wikipedia.org/wiki/Peer-to-peer_web_hosting
            - This requires decentralizing DNS.
            - This requires replacing all browsers.
            - IPFS? IPNS?
            - https://blog.neocities.org/blog/2015/09/08/its-time-for-the-distributed-web.html
