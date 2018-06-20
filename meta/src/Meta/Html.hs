{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Meta.Html where

import Prelude hiding (seq)

data Html a
    = Empty
    | Embed a
    | Seq (Html a) (Html a)
    | Text String
    | Elm Name [Atr] (Html a)
    deriving (Show, Read)

instance Monoid (Html a) where
    mempty = Empty
    mappend = Seq

-- | Convenience constructor for 'Elm'.
elm :: Name -- ^ tag name
    -> [Atr] -- ^ attributes
    -> [Html a] -- ^ children
    -> Html a
elm name atrs children = Elm name atrs (mconcat children)

type Name = String

type Value = String

data Atr = MkAtr Name Value deriving (Show, Read)

-- * Fold

fold :: (Monoid a) => (String -> a) -> Html a -> a
fold string html = case html of
    Empty -> mempty
    Embed a -> a
    Seq x y -> recur x `mappend` recur y
    Text s -> string (escape s)
    Elm name atrs body ->
        string ("<" ++ name ++ unwords (map render_atr atrs) ++ ">")
        `mappend` recur body
        `mappend` string ("</" ++ name ++ ">")
    where
        recur = fold string
        render_atr :: Atr -> String
        render_atr (MkAtr name value) =
            escape name ++ "=" ++ escape value

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
