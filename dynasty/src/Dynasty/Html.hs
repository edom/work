{-# OPTIONS -fno-warn-missing-signatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- |
HTML views.
-}
module Dynasty.Html
where

import Prelude
    (
        ($)
        , (++)
        , (.)
        , (>>)
        , Double
        , Int
        , Show
        , String
        , floor
        , show
    )

import Data.String (IsString, fromString)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import qualified Control.Monad as M

import qualified Text.Blaze.Html5.Attributes as A

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P

type ToHtml a = a -> Html

showOff :: (Show a) => ToHtml a
showOff = fromString . show

date :: ToHtml D.Date
date = fromString . D.print

dbl :: Double -> String
dbl = show . (floor :: Double -> Int)

showd :: (IsString a) => Double -> a
showd = fromString . dbl

person x =
    table ! class_ "person" $ do
        row "Id" (showOff pid)
        row "Sex" (showOff $ P.sex x)
        row "Honorified Name" (a ! href (fromString $ "/person/" ++ show pid) $ fromString $ P.honorifiedName x)
        row "Father Id / Mother Id" $ fromString $ show (P.fatherId x) ++ " / " ++ show (P.motherId x)
        row "Born" (date $ P.born x)
        row "Gold / Prestige / Piety" $ fromString $ dbl (P.gold x) ++ " / " ++ dbl (P.prestige x) ++ " / " ++ dbl (P.piety x)
        tr $ do
            td ! A.title "Diplomacy / Martial / Stewardship / Intrigue / Learning" $ "D / M / S / I / L"
            td $ fromString $ show (P.diplomacy x) ++ " / " ++ show (P.martial x)
                ++ " / " ++ show (P.stewardship x) ++ " / " ++ show (P.intrigue x)
                ++ " / " ++ show (P.learning x)
        row "Titles" $ ul $ M.mapM_ (li . fromString) (P.formattedTitlesOf x)
    where
        pid = P.id x
        row y z = tr (td y >> td z)
