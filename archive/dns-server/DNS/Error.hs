module DNS.Error
(
    checkEither
)
where

import qualified System.IO.Error as IE

checkEither :: Either String a -> IO a
checkEither = either (IE.ioError . IE.userError) return
