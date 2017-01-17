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

data Sex
    = Male
    | Female
    deriving (Eq, Show)

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

formatLevel :: Sex -> Level -> String
formatLevel sex level =
    case sex of
        Male -> show level
        Female -> case level of
            Baron -> "Baroness"
            Count -> "Countess"
            Duke -> "Duchess"
            King -> "Queen"
            Emperor -> "Empress"

format :: Sex -> Title -> String
format sex (MkTitle level place) = formatLevel sex level ++ " of " ++ place
