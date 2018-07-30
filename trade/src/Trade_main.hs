module Trade_main (
    ebnis_server
) where

import qualified Ebnis_server as S

ebnis_server :: IO ()
ebnis_server = S.main
