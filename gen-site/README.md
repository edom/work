- Plans
    - Modernize this (it uses ghc 7, cabal 1, pandoc, hakyll).
    - Things that must be done anyway.
        - Pick or make a lightweight semantic markup with fixed ontology.
            - https://en.wikipedia.org/wiki/Lightweight_markup_language
            - Which markup language?
                - reST
                - [Textile](https://en.wikipedia.org/wiki/Textile_(markup_language))
                - S-expression
                - not Markdown?
                - https://tiddlywiki.com/static/WikiText.html
                - https://hackage.haskell.org/package/mmark
            - Which tool?
                - pandoc
                - hakyll
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
