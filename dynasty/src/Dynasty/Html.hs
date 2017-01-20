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
        , (.)
        , (>>)
        , Double
        , Int
        , Show
        , floor
        , show
    )

import Data.String (fromString)

import Text.Blaze.Html5
import Text.Blaze.Html5.Attributes

import qualified Control.Monad as M

import qualified Dynasty.Date as D
import qualified Dynasty.Person as P

type ToHtml a = a -> Html

showOff :: (Show a) => ToHtml a
showOff = fromString . show

date :: ToHtml D.Date
date = fromString . D.print

showd = fromString . show . (floor :: Double -> Int)

person x =
    table ! class_ "person" $ do
        row "Id" (showOff $ P.id x)
        row "Sex" (showOff $ P.sex x)
        row "Honorified Name" (fromString $ P.honorifiedName x)
        row "Father Id" (showOff $ P.fatherId x)
        row "Mother Id" (showOff $ P.motherId x)
        row "Born" (date $ P.born x)
        row "Gold" (showd $ P.gold x)
        row "Prestige" (showd $ P.prestige x)
        row "Piety" (showd $ P.piety x)
        row "Diplomacy" (showOff $ P.diplomacy x)
        row "Martial" (showOff $ P.martial x)
        row "Stewardship" (showOff $ P.stewardship x)
        row "Intrigue" (showOff $ P.intrigue x)
        row "Learning" (showOff $ P.learning x)
        row "Titles" $ ul $ M.mapM_ (li . fromString) (P.formattedTitlesOf x)
    where
        row y z = tr (td y >> td z)
