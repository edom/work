-- | STOMP (Streaming Text Oriented Message Protocol).
module Meta.Stomp (
    Frame(..)
    , Command
    , Header
    , get_command
    , get_headers
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

type Command = String

mk_frame :: Command -> [Header] -> Frame
mk_frame = MkFrame

type Header = (String, String)
