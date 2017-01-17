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

    deriving (Eq, Show)

data Group
    = Celtic
    | WestGermanic
    deriving (Eq, Show)
