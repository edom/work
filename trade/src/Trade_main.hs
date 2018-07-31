module Trade_main (
    ebnis_client
    , ebnis_server
) where

import qualified Ebnis_client as C
import qualified Ebnis_server as S

-- | See 'C.main' in "Ebnis_client".
ebnis_client :: IO ()
ebnis_client = C.main

-- | See 'S.main' in "Ebnis_server".
ebnis_server :: IO ()
ebnis_server = S.main
