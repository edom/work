{- |
This reexports some of the "Text.PrettyPrint" module of the @pretty@ package.
-}
module Meta.Pretty (
    P.Doc
    , P.render
    , P.text
    , (P.<>)
    , (P.<+>)
    , (P.$$)
    , P.nest
    , P.doubleQuotes
    , P.parens
) where

import qualified Text.PrettyPrint as P
