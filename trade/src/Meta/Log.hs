{- |
Logging.
-}
module Meta.Log (
    Monad_log(..)
) where

import Prelude ()
import Meta.Prelude

import qualified Control.Concurrent as C
import qualified Control.Monad.Reader as R
import qualified Data.Time as Time

-- | A 'Monad' that can log messages.
class (Monad m) => Monad_log m where

    -- | Write the message.
    -- The message should be something that you will want to read.
    log :: String -> m ()

instance Monad_log IO where
    log msg = do
        utc <- Time.getCurrentTime
        tid <- C.myThreadId
        let time = Time.formatTime Time.defaultTimeLocale "%FT%T%03Q" utc
        putStrLn $ time ++ "\t[" ++ show tid ++ "]\t" ++ msg

instance (MonadIO m) => Monad_log (R.ReaderT r m) where
    log msg = liftIO $ log msg
