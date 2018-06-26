module Meta.CalVal where

import Prelude hiding (not)

import qualified Prelude as P

data Val
    = Error [String]
    | Unit
    | Bool Bool
    | Int Int
    | String String
    deriving (Read, Show)

plus :: Val -> Val -> Val
plus (Error a) (Error b) = Error (a ++ b)
plus (Error a) _ = Error a
plus _ (Error b) = Error b
plus (Int a) (Int b) = Int (a + b)
plus (String a) (String b) = String (a ++ b)
plus a b = Error ["Meta.CalVal.plus: (" ++ show a ++ ") (" ++ show b ++ ")"]

eq :: Val -> Val -> Val
eq (Error a) (Error b) = Error (a ++ b)
eq (Error a) _ = Error a
eq _ (Error b) = Error b
eq (Int a) (Int b) = Bool (a == b)
eq (Bool a) (Bool b) = Bool (a == b)
eq a b = Error ["Meta.CalVal.eq: (" ++ show a ++ ") (" ++ show b ++ ")"]

not :: Val -> Val
not (Error a) = Error a
not (Bool a) = Bool (P.not a)
not a = Error ["Meta.CalVal.not: (" ++ show a ++ ")"]

eval :: Val -> Val
eval = id

print :: Val -> String
print (Bool x) = show x
print (Int x) = show x
print val = show val
