module Dynasty.Multiplay.Table
(
    Table(..)
    , Id
    , makeId
    , makeIdM
    , makeRandomId
)
where

import Data.Word (Word8)
import Prelude hiding (id)

import qualified Control.Monad.IO.Class as I
import qualified Control.Monad as M

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

import qualified Data.ByteString.Base64.URL as BU

import qualified Dynasty.Random as R
import qualified Dynasty.State as S

data Table
    = MkTable
    {
        id :: Id
        , state :: S.State
    }

type Id = String

makeId
    :: [Word8] -- ^ Finite list of random bytes.
    -> String -- ^ URL-safe

makeId = BC.unpack . BU.encode . B.pack

makeIdM
    :: (Monad m)
    => m Word8 -- ^ Generate a random byte.
    -> m String

makeIdM generateRandomByte =
    makeId <$> M.replicateM 6 generateRandomByte

makeRandomId :: (I.MonadIO m) => m String
makeRandomId = makeIdM R.randomWord8
