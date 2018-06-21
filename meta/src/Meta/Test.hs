module Meta.Test where

import qualified Meta.Html as Html
import qualified Meta.Java as J
import qualified Meta.JavaWebApp as JWA
import qualified Meta.User as U

test0 :: IO ()
test0 = mapM_ print stas
    where
        stas = JWA.content_to_java_sta (J.e_name "output") content
        content = U.html [
                U.h1 [U.atr "foo" "bar", U.text "text"]
            ]

test1 :: IO ()
test1 = putStrLn string
    where
        string = Html.fold "" (++) id html
        html = Html.elm "h1" [
                Html.EAtr "foo" "bar"
                , Html.EAtr "baz" "qux"
                , Html.EAtr "trick" "<&\">"
                , Html.EAtr "&invalid" ""
                , Html.Text "text"
            ]
