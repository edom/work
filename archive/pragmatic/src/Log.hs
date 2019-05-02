{- |
Logging.
-}
module Log where

import Prelude (
    ($)
    , (++)
    , (.)
    , (<$>)
    , (>>)
    , Bool
    , Bounded
    , Enum
    , Eq
    , IO
    , Ord
    , Read
    , Show
    , String
    , map
    , return
    , show
    )

import qualified Data.Char as Ch
import qualified Data.Time as Ti
import qualified Text.Printf as Pf
import qualified System.IO as SI

import qualified Control.Monad.IO.Class as Ic

import qualified Mutex as Mu

-- * Constructors

{- $
All constructors produce 'Log's that are not thread-safe.
-}

{- |
This is a logger that throws away everything sent to it.
-}
discard :: Log
discard _ _ = return ()

{- |
This is a logger that prepends 'time_severity'
and writes to standard output.
-}
stdout :: Log
stdout = time_severity stdout_raw

{- |
A logger that prepends 'time_severity' and writes to the handle.
-}
handle :: SI.Handle -> Log
handle = time_severity . handle_raw

-- ** Rarely-used low-level constructors

{- |
A logger that writes to standard output.
-}
stdout_raw :: Log
stdout_raw = handle_raw SI.stdout

{- |
A logger that writes to the handle.
-}
handle_raw :: SI.Handle -> Log
handle_raw hnd _sev msg = do
    SI.hPutStrLn hnd msg
    SI.hFlush hnd

-- ** IO constructors

{- |
This appends to the given file.

This 'Log' does not provide any way of closing the file.
-}
file :: SI.FilePath -> IO Log
file path =
    handle <$> SI.openFile path SI.AppendMode

{- |
This appends to the given file, and closes the file
after the program has finished using the 'Log'.
-}
with_file :: SI.FilePath -> (Log -> IO a) -> IO a
with_file path use =
    SI.withFile path SI.AppendMode $ use . handle

-- * Combinators

{- |
@and log0 log1@ is a logger that sends everything sent to it to both @log0@ and @log1@.
-}
and :: Log -> Log -> Log
and log0 log1 sev msg =
    log0 sev msg >> log1 sev msg

{- |
A logger that prepends the current UTC time
and the severity to the message.
-}
time_severity :: Log -> Log
time_severity parent sev msg = do
    time <- Ti.getCurrentTime
    let s_time = Ti.formatTime Ti.defaultTimeLocale "%Y-%m-%d %H:%M:%S UTC" time
        s_sev = map Ch.toUpper $ show sev
        longer_msg = Pf.printf "%s %-5s %s" s_time s_sev msg
    parent sev longer_msg

{- |
A logger that can change the message sent to its parent logger.
-}
map_msg :: (String -> String) -> Log -> Log
map_msg fun parent sev msg = parent sev (fun msg)

{- |
A logger that prepends the prefix to its messages.
-}
prefix :: String -> Log -> Log
prefix p = map_msg (p ++)

-- ** Conditional

{- |
@if_level pred tru fls@ is a logger that sends to @tru@ all messages
whose severity satisfies @pred@, and everything else to @fls@.
-}
if_level :: (Severity -> Bool) -> Log -> Log -> Log
if_level pred logtrue logfalse sev =
    (if pred sev then logtrue else logfalse) sev

-- ** Synchronization for thread safety

{- |
Convenience function for 'synced_to' that creates a new mutex.
-}
synced :: Log -> IO Log
synced parent = do
    mutex <- Mu.new
    return $ synced_to mutex parent

{- |
A logger that holds the mutex (semaphore) while calling the parent log.

You need this if multiple threads use the same log;
otherwise the output will be mixed up.
-}
synced_to :: Mu.Mutex -> Log -> Log
synced_to mutex parent level message =
    Mu.with mutex $ parent level message

-- * Convenience functions

-- | Log a message with 'Trace' severity.
trace :: (Ic.MonadIO m) => Log -> String -> m ()
trace this msg = Ic.liftIO $ this Trace msg

-- | Log a message with 'Debug' severity.
debug :: (Ic.MonadIO m) => Log -> String -> m ()
debug this msg = Ic.liftIO $ this Debug msg

-- | Log a message with 'Info' severity.
info :: (Ic.MonadIO m) => Log -> String -> m ()
info this msg = Ic.liftIO $ this Info msg

-- | Log a message with 'Warn' severity.
warn :: (Ic.MonadIO m) => Log -> String -> m ()
warn this msg = Ic.liftIO $ this Warn msg

-- | Log a message with 'Error' severity.
error :: (Ic.MonadIO m) => Log -> String -> m ()
error this msg = Ic.liftIO $ this Error msg

-- * Definitions

{- |
You should not depend on the concrete implementation
(the fact that 'Log' is a type synonym for a function type).

In the future this may be replaced with a data type.
-}
type Log = Severity -> String -> IO ()

{- |
Ordering:
We have @'Trace' '<' 'Debug'@, and
@'Debug' '<' 'Info'@,
and so on.
-}
data Severity
    = Trace
    | Debug
    | Info
    | Warn
    | Error
    deriving (Read, Show, Eq, Ord, Enum, Bounded)
