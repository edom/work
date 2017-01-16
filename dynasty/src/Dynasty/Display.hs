module Dynasty.Display where

import qualified UI.HSCurses.Curses as C

data CharIo
    = MkCharIo
    {
        puts :: String -> IO ()
        , getch :: IO (Maybe CharInput)
    }

curses :: C.Window -> CharIo
curses window =
    MkCharIo
        (C.wAddStr window)
        getch_
    where
        getch_ = do
            eKey <- C.getch
            return $ case eKey of
                _ | eKey == C.cERR -> Nothing
                _ -> Just $ case C.decodeKey eKey of
                    C.KeyChar c -> Char c
                    _ -> Unknown $ fromIntegral eKey

data CharInput
    = Char Char
    | Unknown Int
    deriving (Show, Eq)

isChar :: Char -> CharInput -> Bool
isChar a (Char b) = a == b
isChar _ _ = False

asChar :: CharInput -> Maybe Char
asChar (Char x) = Just x
asChar _ = Nothing
