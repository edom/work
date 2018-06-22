module Meta.CalVal where

data Val
    = Error String
    | Bool Bool
    | Int Int
    deriving (Read, Show)

plus :: Val -> Val -> Val
plus (Int a) (Int b) = Int (a + b)
plus a b = Error $ "Meta.CalVal.plus: (" ++ show a ++ ") (" ++ show b ++ ")"

eq :: Val -> Val -> Val
eq (Int a) (Int b) = Bool (a == b)
eq (Bool a) (Bool b) = Bool (a == b)
eq a b = Error $ "Meta.CalVal.eq: (" ++ show a ++ ") (" ++ show b ++ ")"

print :: Val -> String
print (Bool x) = show x
print (Int x) = show x
print val = show val
