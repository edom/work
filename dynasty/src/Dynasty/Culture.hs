module Dynasty.Culture where

type Name = String

data Culture

    = None
    
    | Irish
    | Scottish
    | Pictish
    | Welsh
    | Breton

    | English
    | AngloSaxon
    | Saxon
    | Frisian
    | Dutch

    | Norse
    | Swedish
    | Norwegian
    | Danish

    deriving (Eq, Show)

data Group
    = Celtic
    | WestGermanic
    | NorthGermanic
    deriving (Eq, Show)
