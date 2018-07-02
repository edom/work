module Meta.Html where

import Prelude hiding (concat, seq)

{- |
Thoughts on 'Elm' constructor:

An attribute is a child node of an element.

Pros:

* This gives rise to uniform syntax for adding attributes and elements.
Example: @anElem [atr key1 val1, ..., childElem1, ...]@ is more uniform than @anElem [atr key1, val1] [childElem1, ...]@.
See 'mk_atr'.

Cons:

* This complicates rendering.
See 'get_atrs'.

We could define @Elm Name [Atr] a@ instead of @Elm Name (Html a)@.
An attribute would then be a /property/ of an element instead of a /child/ of an element.
-}
data Html a
    = Empty
    | Pure a
    | Seq (Html a) (Html a)
    | Text String -- ^ will be escaped
    | EAtr Name Value -- ^ attribute node
    | Elm Name (Html a) -- ^ name, first child
    deriving (Read, Show)

type Name = String

type Value = String

-- | Convenience constructor for 'Elm'.
elm :: Name -- ^ tag name
    -> [Html a] -- ^ children
    -> Html a

elm name children = Elm name (concat children)

type Atr = (Name, Value)

mk_atr :: Name -> Value -> Atr
mk_atr = (,)

atr_get_name :: Atr -> Name
atr_get_name = fst

atr_get_value :: Atr -> Value
atr_get_value = snd

get_atrs :: Html a -> [Atr]
get_atrs x = [ mk_atr nam val | EAtr nam val <- get_children x ]

get_children :: Html a -> [Html a]
get_children x = case x of
    Elm _ child -> listify_level child
    _ -> []
    where
        listify_level :: Html a -> [Html a]
        listify_level node = case node of
            Seq a b -> listify_level a ++ listify_level b
            y -> [y]

-- * Fold

fold
    :: a -- ^ if 'Empty', monoid identity element
    -> (a -> a -> a) -- ^ if 'Seq', monoid append operation
    -> (String -> a) -- ^ if 'Text'
    -> Html a
    -> a

fold empty_ seq_ string_ = recur
    where
        recur html = case html of
            Empty -> empty_
            Pure a -> a
            Seq x y -> seq_ (recur x) (recur y)
            Text s -> string_ (escape s)
            this@(Elm name child) ->
                let
                    atrs = get_atrs this
                    s_atrs = unwords ("" : map render_atr atrs)
                in
                    string_ ("<" ++ name ++ s_atrs ++ ">")
                    `seq_` recur child
                    `seq_` string_ ("</" ++ name ++ ">")
            EAtr _ _ -> empty_
        render_atr :: Atr -> String
        render_atr atr =
            escape (atr_get_name atr) ++ "=\"" ++ escape (atr_get_value atr) ++ "\""

-- * Escape

escape :: String -> String
escape = concatMap esc
    where
        esc c = case c of
            '"' -> "&quot;"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            _ -> [c]

-- * Monoid

empty :: Html a
empty = Empty

append :: Html a -> Html a -> Html a
append = Seq

concat :: [Html a] -> Html a
concat = foldr append empty
