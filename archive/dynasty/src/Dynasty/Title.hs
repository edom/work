{-# OPTIONS -fno-warn-name-shadowing #-}

module Dynasty.Title where

import qualified Dynasty.Level as L

type Place = String

data Title
    = MkTitle
    {
        level :: L.Level
        , place :: Place
    }
    deriving (Eq, Ord, Show)

baronOf :: Place -> Title
baronOf = MkTitle L.Baron

countOf :: Place -> Title
countOf = MkTitle L.Count

dukeOf :: Place -> Title
dukeOf = MkTitle L.Duke

kingOf :: Place -> Title
kingOf = MkTitle L.King

emperorOf :: Place -> Title
emperorOf = MkTitle L.Emperor

format :: L.Sex -> Title -> String
format sex (MkTitle level place) = L.format sex level ++ " of " ++ place
