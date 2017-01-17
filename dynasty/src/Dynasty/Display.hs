module Dynasty.Display where

import qualified Foreign.C as F

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
        (wAddStr window)
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

wAddStr :: C.Window -> String -> IO ()
wAddStr window string = do
    _ <- F.withCStringLen string $ \ (cstr, len) ->
        waddnstr window  cstr (fromIntegral len)
    return ()

foreign import ccall safe waddnstr :: C.Window -> F.CString -> F.CInt -> IO F.CInt
