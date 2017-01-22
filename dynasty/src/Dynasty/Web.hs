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

import qualified Dynasty.Multiplay.Parlor as MP
import qualified Dynasty.Multiplay.Table as MT
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
    W.get "/" $ W.html $ BT.renderHtml H.start
    W.post "/new" $ do
        tableId <- MT.makeRandomId
        seeOther $ "/table/" ++ tableId
    W.get "/table/:tableId" $ do
        tableId <- W.param "tableId"
        let tableUrl = "/table/" ++ tableId
        people <- getPeople
        today <- getToday
        events <- getEvents
        W.html $ BT.renderHtml $ H.page today "Dynasty Simulator" $ do
            B.form B.! A.action (DS.fromString $ tableUrl ++ "/end-day") B.! A.method "POST" $ do
                B.input B.! A.type_ "submit" B.! A.value "Next day"
            B.h2 "Events"
            B.ul $ M.forM_ events $ B.li . DS.fromString
            B.h2 "People"
            M.mapM_ (H.person tableUrl) people
    W.get "/table/:tableId/person/:id" $ do
        tableId <- W.param "tableId"
        let tableUrl = "/table/" ++ tableId
        pid <- W.param "id"
        people <- findPerson pid
        today <- getToday
        W.html $ BT.renderHtml $ H.page today "Person" $ do
            B.form B.! A.action (DS.fromString $ tableUrl ++ "/end-day") B.! A.method "POST" $ do
                B.input B.! A.type_ "submit" B.! A.value "Next day"
            M.mapM_ (H.person tableUrl) people
    W.post "/table/:tableId/end-day" $ do
        tableId <- W.param "tableId"
        let tableUrl = "/table/" ++ tableId
        endDay
        seeOther tableUrl
    where
        findPerson = S.findPerson server
        getPeople = S.getPeople server
        endDay = S.endDay server
        getToday = S.getToday server
        getEvents = S.getEvents server
        seeOther url = do
            W.status HS.seeOther303
            W.setHeader "Location" $ DS.fromString url
