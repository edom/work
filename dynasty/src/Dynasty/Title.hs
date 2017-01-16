module Dynasty.Title where

type Place = String

data Title
    = MkTitle Level Place
    deriving (Show)

data Level
    = Baron
    | Count
    | Duke
    | King
    | Emperor
    deriving (Eq, Ord, Show)

baronOf :: Place -> Title
baronOf = MkTitle Baron

countOf :: Place -> Title
countOf = MkTitle Count

dukeOf :: Place -> Title
dukeOf = MkTitle Duke

kingOf :: Place -> Title
kingOf = MkTitle King

emperorOf :: Place -> Title
emperorOf = MkTitle Emperor

format :: Title -> String
format (MkTitle level place) = show level ++ " of " ++ place
