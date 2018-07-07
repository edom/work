module Meta.PegGenLoc (
    Loc
    , get_row
    , get_col
    , Locd(..)
    , locd
    , begin
    , char
    , newline
    , display
    , display_short
) where

import Prelude hiding (print)

data Loc = MkLoc {
        _row :: Int
        , _col :: Int
    } deriving (Read, Show)

get_row :: Loc -> Int
get_row = _row

get_col :: Loc -> Int
get_col = _col

data Locd a = MkLocd {
        _loc :: Loc
        , _dat :: a
    } deriving (Read, Show)

locd :: Loc -> a -> Locd a
locd = MkLocd

begin :: Loc
begin = MkLoc 1 1

char :: Loc -> Loc
char (MkLoc r c) = MkLoc r (c + 1)

newline :: Loc -> Loc
newline (MkLoc r _) = MkLoc (r + 1) 1

display :: Loc -> String
display (MkLoc r c) = "line " ++ show r ++ " column " ++ show c

display_short :: Loc -> String
display_short (MkLoc r c) = show r ++ ":" ++ show c
