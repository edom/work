module Meta.Csv where

import qualified System.IO.Error as IE

import qualified Text.CSV as Csv

type Col = String

type Row = [Col]

read_file :: FilePath -> IO [Row]
read_file path = do
    e <- Csv.parseCSVFromFile path
    case e of
        Left err -> IE.ioError $ IE.userError $ show err
        Right x -> return x

{-
-- cassava

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Csv as Csv
import qualified Data.Vector as V

read_file :: FilePath -> IO [[String]]
read_file path = do
    bs <- BSL.fromStrict <$> BS.readFile path
    case Csv.decode Csv.NoHeader bs of
        Left msg -> IE.ioError $ IE.userError msg
        Right cols -> return $ V.toList cols
-}
