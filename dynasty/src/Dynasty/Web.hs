{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Dynasty.Web
(
    Logic
    ,
    serve
)
where

import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as I
import qualified Data.String as DS

import qualified Network.HTTP.Types.Status as HS

import qualified Text.Blaze.Html5 as B
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Text as BT

import qualified Web.Scotty as W

import qualified Dynasty.Date as D
import qualified Dynasty.Server as S
import qualified Dynasty.Html as H

type Logic = forall m. (I.MonadIO m) => S.Server m

{- |
Provide a HTTP interface to the game.
-}
serve :: Logic -> IO ()
serve server = W.scotty 8008 $ do
    W.get "/static/style.css" $ do
        W.setHeader "Content-Type" "text/css; charset=UTF-8"
        W.file "static/style.css"
    W.get "/" $ do
        people <- getPeople
        today <- getToday
        W.html $ BT.renderHtml $ B.docTypeHtml $ do
            B.head $ do
                B.title "Dynasty Simulator"
                B.link B.! A.rel "stylesheet" B.! A.href "static/style.css"
            B.body $ do
                B.div $ do
                    B.span "Today is "
                    B.span $ DS.fromString $ D.print today
                B.form B.! A.action "/end-day" B.! A.method "POST" $ do
                    B.input B.! A.type_ "submit" B.! A.value "Next day"
                B.h1 "People"
                M.mapM_ H.person people
    W.post "/end-day" $ do
        endDay
        W.status HS.seeOther303
        W.setHeader "Location" "/"
    where
        getPeople = S.getPeople server
        endDay = S.endDay server
        getToday = S.getToday server
