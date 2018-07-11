module Meta.YiMain (
    main
) where

import Prelude ()
import Meta.Prelude

import qualified Meta.Yi as Y

main :: IO ()
main = do
    conf <- Y.makeConfig config
    Y.startEditor conf

config :: Y.ConfigM ()
config = do
    Y.configureVty
    Y.globalBindKeys $ do
        _ <- Y.match (Y.func 4) <|> Y.match (Y.ctrl $ Y.char 'q')
        Y.quitEditor
