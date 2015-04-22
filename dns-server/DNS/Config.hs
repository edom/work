module DNS.Config
(
    -- * Projections
    port
    , answerMinTtl
    -- * Creation
    , fromArgs
    , defConfig
    -- * Types
    , Config
)
where

import qualified Text.Read as R

import qualified Network.Socket as S

data Config
    = MkConfig
    {
        _port :: S.PortNumber
        , _answerMinTtl :: Maybe Int
    }
    deriving (Show)

port :: Config -> S.PortNumber
port = _port

answerMinTtl :: Config -> Maybe Int
answerMinTtl = _answerMinTtl

defConfig :: Config
defConfig = MkConfig
    {
        _port = 1053
        , _answerMinTtl = Nothing
    }

fromArgs :: [String] -> Either String Config
fromArgs = loop defConfig
    where
        loop c list = case list of
            [] -> return c
            "--port" : port_ : rest -> R.readEither port_ >>= \ p -> loop c { _port = intPort p } rest
            "--answer-min-ttl" : s : rest -> R.readEither s >>= \ v -> loop c { _answerMinTtl = Just v } rest
            h : _ -> Left $ "invalid argument: " ++ h
        intPort :: Int -> S.PortNumber
        intPort = fromIntegral
