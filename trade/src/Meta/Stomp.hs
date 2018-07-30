{-# LANGUAGE RecordWildCards #-}

-- | STOMP (Streaming Text Oriented Message Protocol).
module Meta.Stomp (
    Frame(..)
    , Command
    , Header
    , Key
    , Value
    , get_command
    , get_headers
    , get_header
    , mk_frame
) where

data Frame
    = MkFrame {
        _command :: Command
        , _headers :: [Header]
    } deriving (Read, Show)

get_command :: Frame -> Command
get_command = _command

get_headers :: Frame -> [Header]
get_headers = _headers

get_header :: (Monad m) => Key -> Frame -> m Value
get_header key MkFrame{..} =
    from_list $ map snd $ filter (\ (k, _) -> key == k) _headers
    where
        from_list :: (Monad m) => [a] -> m a
        from_list [] = fail $ "Meta.Stomp.get_header: No such key: " ++ key
        from_list (x : _) = return x

type Command = String

mk_frame :: Command -> [Header] -> Frame
mk_frame = MkFrame

type Key = String

type Value = String

type Header = (Key, Value)
