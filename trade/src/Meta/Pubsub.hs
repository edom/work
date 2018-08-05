{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Some notes about this event bus (publish-subscribe):

* This is not persistent.

* This is unbuffered.
New subscribers don't see old messages.

* All publishers and subscribers must be in the same operating system process.
-}
module Meta.Pubsub (
    -- * Server
    Running_forum
    , start_forum
    , stop_forum
    , publish
    , subscribe
    -- * Topic
    , Topic
    -- * Subscription
    , Name
    , Subscription
) where

import Prelude (error, seq)
import Meta.Prelude

import qualified Control.Concurrent.STM as S
import qualified Meta.Map as Map

{- |
A forum is a collection of 'Topic's.

To create a new empty forum, use 'empty_forum'.

To use forum, you must call 'garbage_collect' as prescribed in its documentation.

The type parameter @a@ is the message type.
-}
data Forum a = MkForum {
        _forum_topic_map :: Map.Map Name (Topic a)
    }

{- |
To get a topic (or create it if doesn't exist), use 'get_topic'.
-}
data Running_forum a = MkRunningForum {
        _rf_stop :: IO ()
        , _rf_publish :: Name -> a -> IO ()
        , _rf_subscribe :: Name -> IO (Subscription a)
    }

{- |
Start a forum server.

We aren't sure about the correctness of this.
We recommend that you restart your program periodically.
-}
start_forum :: IO (Running_forum a)
start_forum = do
    lock <- new_lock
    var_running <- newIORef True
    var_forum <- newIORef empty_forum
    gc_thread <- forkIO $ do
        forever $ do
            let gc_period_microseconds = 5000000
            threadDelay gc_period_microseconds
            with_lock lock $ do
                forum_0 <- readIORef var_forum
                forum_1 <- garbage_collect forum_0
                atomicModifyIORef' var_forum $ \ forum -> (forum_1, ())
    let stop_ = with_lock lock $ do
            killThread gc_thread
            set var_running False
            set var_forum empty_forum
        publish_ name message = with_lock lock $ do
            error "not implemented"
        subscribe_ name = with_lock lock $ do
            forum <- readIORef var_forum
            error "not implemented"
    return $ MkRunningForum stop_ publish_ subscribe_
    where
        set var val = do
            atomicModifyIORef' var $ \ _ -> (val, ())

stop_forum :: Running_forum a -> IO ()
stop_forum = _rf_stop

publish :: Running_forum a -> Name -> a -> IO ()
publish = _rf_publish

subscribe :: Running_forum a -> Name -> IO (Subscription a)
subscribe = _rf_subscribe

{-
instance (MonadIO m) => Monad_forum m a where
    subscribe (MkTopic chan) = MkSubscription <$> S.dupTChan chan
    publish (MkTopic chan) msg = S.writeTChan chan msg
-}

-- | An empty forum.
empty_forum :: Forum a
empty_forum = MkForum Map.empty

{- |
You must call this periodically, and replace your forum reference with the returned forum.

Otherwise, you will leak memory.
-}
garbage_collect :: Forum a -> IO (Forum a)
garbage_collect forum@MkForum{..} = do
    tm_live <- mapM gc_topic _forum_topic_map
    let tm_inhabited = Map.filter topic_is_inhabited tm_live
    return forum {
            _forum_topic_map = tm_inhabited
        }
    where

        gc_topic :: Topic a -> IO (Topic a)
        gc_topic topic@MkTopic{..} = do
            alive_subs <- filterM is_alive _topic_subscriptions
            return topic { _topic_subscriptions = alive_subs }

        is_alive :: Weak a -> IO Bool
        is_alive w = isJust <$> deRefWeak w

        topic_is_inhabited :: Topic a -> Bool
        topic_is_inhabited MkTopic{..} = not $ null _topic_subscriptions

-- | Topic name.
type Name = ByteString

{- |
A topic is a collection of subscribers.

A topic is something that you can 'publish' to or 'subscribe' to.
-}
data Topic a = MkTopic {
        -- This might should be a Weak (WritableChan a)
        _topic_chan :: S.TChan a -- ^ must be obtained from 'S.newBroadcastTChan'
        , _topic_subscriptions :: [Weak (Subscription a)]
    }

{- |
This is for keeping track of live topics so that dead topics can be garbage-collected.

Clients can disconnect unexpectedly.
-}
data Subscription a = MkSubscription {
        _chan :: S.TChan a
    }

get_topic :: Name -> Forum a -> S.STM (Topic a, Forum a)
get_topic name forum = do
    topic_0 <- case Map.lookup name $ _forum_topic_map forum of
        Just topic -> return topic
        Nothing -> new_topic
    let forum_0 = forum {
                _forum_topic_map = Map.insert name topic_0 $ _forum_topic_map forum
            }
    return (topic_0, forum_0)

new_topic :: S.STM (Topic a)
new_topic = do
    chan <- S.newBroadcastTChan
    return $ MkTopic chan []
