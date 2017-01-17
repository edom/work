module Dynasty.Level where

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

format :: Sex -> Level -> String
format sex level =
    case sex of
        Male -> show level
        Female -> case level of
            Baron -> "Baroness"
            Count -> "Countess"
            Duke -> "Duchess"
            King -> "Queen"
            Emperor -> "Empress"
