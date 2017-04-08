module CharacterFrequencyAnalysis
where

import qualified Control.Monad as M
import qualified Data.Char as C
import qualified Data.Ord as O
import qualified Data.List as L

main :: IO ()
main = do
    table <- analyzeFrequency . filter isKanji <$> getContents
    let
        table2 :: [[(Int, Char)]]
        table2 = L.groupBy (\ (a, _) (b, _) -> a == b) table
        table3 :: [(Int, [Char])]
        table3 = map (foldl (\ (count, chars) (n, c) -> (n, chars ++ [c])) (0, [])) table2
        table4 = map (\ (count, chars) -> (count, L.sort chars)) table3
    putStrLn $ L.intercalate " " $ map showEntry table4
    where
        isKanji c = '\x4e00' <= c && c < '\x9fb0'
        showEntry (count, chars) = show count ++ " " ++ chars

analyzeFrequency :: (Ord a) => [a] -> [(Int, a)]
analyzeFrequency =
    L.sort
    |>
    L.group
    |>
    L.map (\ g -> (length g, head g))
    |>
    L.sortBy (O.comparing (O.Down . fst))

infixl 9 |>
f |> g = g . f
