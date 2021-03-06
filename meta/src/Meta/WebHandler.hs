module Meta.WebHandler where

import Prelude ()
import Meta.Prelude

import qualified Meta.CalExp as E
import qualified Meta.CalVal as V
import qualified Meta.Fix as F

data Exp a
    = V V.Val
    | E (E.Exp a)
    | Nil
    | Cons a a
    | Let String a a
    | PostParam a -- ^ get the value of the named HTTP request POST parameter
    | Output a a -- ^ Output headers body: write response body
    deriving (Read, Show)

{- |
An inhabitant of @Lang@ represents a program that
maps an HTTP request into an HTTP response.
-}
type Lang = F.Fix Exp

-- * Constructors

val :: V.Val -> Lang
val = F.In . V

exp :: E.Exp Lang -> Lang
exp = F.In . E

str :: String -> Lang
str = val . V.String

if_ :: Lang -> Lang -> Lang -> Lang
if_ c t f = exp (E.If c t f)

eq :: Lang -> Lang -> Lang
eq a b = F.In (E (E.Eq a b))

nil :: Lang
nil = F.In Nil

-- ** Built-in HTTP actions

output :: Lang -> Lang -> Lang
output headers body = F.In (Output headers body)

post_param :: Lang -> Lang
post_param = F.In . PostParam

-- * Example

example1 :: Lang
example1 = if_ (post_param (str "answer") `eq` str "42") (output nil $ str "foo") (output nil $ str "bar")

example2 :: Lang
example2 = output nil $ if_ (post_param (str "answer") `eq` str "42") (str "foo") (str "bar")
