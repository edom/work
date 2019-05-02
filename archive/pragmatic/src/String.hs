module String where

import qualified Data.Char as Ch
import qualified Data.List as L

{- |
@align_left n s@ adds spaces to the right of @s@
so that the width of the resulting string is not less than @n@.
-}
align_left :: Int -> String -> String
align_left width string =
    string ++ replicate pad ' '
    where
        pad_n = width - length string
        pad =
            if pad_n >= 0
                then pad_n
                else 0

{- |
@wrap max_line_width words@ wrap words into lines
such that the width of each line does not exceed @max_line_width@,
if possible.

If a word is longer than @max_line_width@,
it will be on a line by itself.
-}
wrap :: Int -> [String] -> String
wrap max_line_width words_ =
    unlines $ loop "" words_
    where
        loop "" [] = []
        loop line [] = [line]
        loop line all_words@(word : other_words)
            | new_line_too_long = line : loop "" all_words
            | otherwise = loop new_line other_words
            where
                new_line = line ++ maybe_separator ++ word
                new_line_too_long = length new_line > max_line_width
                -- Avoid separator at beginning of line.
                maybe_separator = if null line then [] else separator
        -- This can be moved out into a parameter.
        separator = " "

{- |
@replace_all src dst str@ replaces each occurrence of @src@ in @str@ with @dst@.
-}
replace_all :: String -> String -> String -> String
replace_all src dst =
    fun
    where
        fun [] = []
        fun str@(h : t) =
            case L.stripPrefix src str of
                Just str2 -> dst ++ fun str2
                _ -> h : fun t

-- * Trim

{- |
Delete leading and trailing spaces.
-}
trim :: String -> String
trim = reverse . dropWhile Ch.isSpace . reverse . dropWhile Ch.isSpace

-- * Case conversion

class Downcase a where
    downcase :: a -> a

instance Downcase Char where
    downcase = Ch.toLower

instance (Downcase a) => Downcase [a] where
    downcase = map downcase

-- * Pretty printing.

class Pretty a where
    pretty :: a -> String

instance (Pretty a) => Pretty [a] where
    pretty = unlines . map pretty
