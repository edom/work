{-# LANGUAGE ForeignFunctionInterface #-}

{- |
Text user interface.
-}
module Dynasty.Display
(
    CharIo

    , puts
    , getch
    , erase
    , refresh

    , curses

    , CharInput

    , isChar

    , asChar

    -- * Reexport

    , I.MonadIO(..)
)
where

import qualified Control.Monad.IO.Class as I
import qualified Foreign.C as F

import qualified UI.HSCurses.Curses as C

data CharIo
    = MkCharIo
    {
        puts_ :: String -> IO ()
        , getch_ :: IO (Maybe CharInput)
        , erase_ :: IO ()
        , refresh_ :: IO ()
    }

puts :: (I.MonadIO m) => CharIo -> String -> m ()
puts d = I.liftIO . puts_ d

getch :: (I.MonadIO m) => CharIo -> m (Maybe CharInput)
getch d = I.liftIO $ getch_ d

erase :: (I.MonadIO m) => CharIo -> m ()
erase d = I.liftIO $ erase_ d

refresh :: (I.MonadIO m) => CharIo -> m ()
refresh d = I.liftIO $ refresh_ d

curses :: C.Window -> CharIo
curses window =
    MkCharIo
        (wAddStr window)
        getch__
        erase__
        refresh__
    where
        getch__ = do
            eKey <- C.getch
            return $ case eKey of
                _ | eKey == C.cERR -> Nothing
                _ -> Just $ case C.decodeKey eKey of
                    C.KeyChar c -> Char c
                    _ -> Unknown $ fromIntegral eKey
        erase__ = C.erase
        refresh__ = C.refresh

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

{- |
This exists because the wAddStr that comes with HSCurses uses 'error'.
-}
wAddStr :: C.Window -> String -> IO ()
wAddStr window string = do
    _ <- F.withCStringLen string $ \ (cstr, len) ->
        waddnstr window  cstr (fromIntegral len)
    return ()

foreign import ccall safe waddnstr :: C.Window -> F.CString -> F.CInt -> IO F.CInt
