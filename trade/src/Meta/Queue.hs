-- | Message queue.
module Meta.Queue (
    -- * Forum
    Forum
    , empty_forum
    -- * Message
    , Message
    , mk_message
    , get_content
    , Lifespan_second
    -- * Writing
    , Name
    , forum_publish
    -- * Reading
    , subscribe
    -- * Queue
    , Queue
    , empty_queue
    , queue_add
    -- * Garbage collection
    , clean_forum
) where

import Prelude ()
import Meta.Prelude

import qualified Meta.Map as M

-- | A forum is a set of named 'Queue's.
data Forum a = MkForum {
        _f_queues :: M.Map Name (Queue a)
    } deriving (Read, Show)

empty_forum :: Forum a
empty_forum = MkForum M.empty

type Name = ByteString

-- | A queue is a list of 'Message's.
data Queue a = MkQueue {
        _q_messages :: [Message a]
    } deriving (Read, Show)

empty_queue :: Queue a
empty_queue = MkQueue []

clean_forum :: Forum a -> IO (Forum a)
clean_forum forum = do
    now <- getCurrentTime
    return forum {
            _f_queues = M.filter (not . queue_empty) $ M.map (clean_queue now) (_f_queues forum)
        }

queue_empty :: Queue a -> Bool
queue_empty queue = null (_q_messages queue)

clean_queue :: Instant -> Queue a -> Queue a
clean_queue now queue = queue {
        _q_messages = filter (\ m -> _m_death m <= now) $ _q_messages queue
    }

get_or_create_queue :: Forum a -> Name -> (Queue a, Forum a)
get_or_create_queue forum name = case M.lookup name (_f_queues forum) of
    Just queue ->
        (queue, forum)
    Nothing ->
        let
            queue_0 = empty_queue
            forum_0 = forum {
                    _f_queues = M.insert name queue_0 (_f_queues forum)
                }
        in
            (queue_0, forum_0)

get_queue_or_empty :: Forum a -> Name -> Queue a
get_queue_or_empty forum qname = maybe empty_queue id $ M.lookup qname (_f_queues forum)

map_queue :: Forum a -> Name -> (Queue a -> Queue a) -> Forum a
map_queue forum name func = forum {
        _f_queues = M.alter update name (_f_queues forum)
    }
    where
        update = return . func . maybe empty_queue id

forum_publish :: Forum a -> Name -> Lifespan_second -> [a] -> IO (Forum a)
forum_publish forum qname lifespan contents = do
    messages <- mapM (mk_message lifespan) contents
    return $ map_queue forum qname (queue_add messages)

subscribe :: Forum a -> Name -> [Message a]
subscribe forum qname = _q_messages $ get_queue_or_empty forum qname

queue_add :: [Message a] -> Queue a -> Queue a
queue_add messages queue = queue { _q_messages = _q_messages queue ++ messages }

{- |
We assume that the system time is monotonic (never goes back).

If time jumps back, some messages may never be read.
-}
type Instant = UTCTime

-- | A message can expire.
data Message a = MkMessage {
        _m_birth :: Instant
        , _m_death :: Instant
        , _m_content :: a
    } deriving (Read, Show)

get_content :: Message a -> a
get_content = _m_content

{- |
'NominalDiffTime' is an instance of 'Num'.
Numeric literals are assumed to be in seconds.
Fractional numbers are possible.
-}
type Lifespan_second = NominalDiffTime

mk_message :: Lifespan_second -> a -> IO (Message a)
mk_message lifespan content = do
    birth <- getCurrentTime
    let death = addUTCTime lifespan birth
    return $ MkMessage birth death content
