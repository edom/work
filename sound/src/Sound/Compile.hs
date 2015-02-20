{- |
There are many ways to define a stream.

This unifies them.
-}
module Sound.Compile
(
    Exp(..)
    , Strm(..)
    , ModuleName
    , BindName
    , simplify
    , list
)
where

type ModuleName = String
type BindName = String

data Exp
    = Int Int
    | Dbl Double
    | Str String
    | Ref ModuleName BindName
    | App Exp Exp
    | Lam Exp
    | Var Int -- ^ de bruijn
    deriving (Read, Show)

-- | An infinite sequence of 'Double's.
data Strm
    = Con Exp
    | Itr Exp Exp
    | Map Exp Strm
    | Zip Exp Strm Strm
    | Out FilePath Strm
    deriving (Read, Show)

infixl 1 `App`

simplify :: Strm -> Strm
simplify s = case s of
    Map f (Con v) -> Con (f `App` v)
    Map f (Map g x) -> simplify $ Map (rtlcompose_ `App` f `App` g) (simplify x)
    -- Map f (Zip g x y) -> simplify $ Zip (\ u v -> f (g u v)) (simplify x) (simplify y)

    Zip f (Con x) (Con y) -> Con (f `App` x `App` y)
    Zip f (Con x) y -> simplify $ Map (f `App` x) (simplify y)
    -- Zip f x (Con y) -> simplify $ Map (flip f y) (simplify x)

    -- Zip f (Map g x) (Map h y) -> simplify $ Zip (\ u v -> f (g u) (h v)) (simplify x) (simplify y)
    -- Zip f (Map g x) y -> simplify $ Zip (\ u v -> f (g u) v) (simplify x) (simplify y)
    -- Zip f x (Map g y) -> simplify $ Zip (\ u v -> f u (g v)) (simplify x) (simplify y)

    _ -> s

rtlcompose_ :: Exp
rtlcompose_ = Ref "Prelude" "(.)"

repeat_ :: Exp
repeat_ = Ref "Prelude" "repeat"

map_ :: Exp
map_ = Ref "Prelude" "map"

zipWith_ :: Exp
zipWith_ = Ref "Prelude" "zipWith"

iterate_ :: Exp
iterate_ = Ref "Prelude" "iterate"

writeFile_ :: Exp
writeFile_ = Ref "System.IO" "writeFile"

list :: Strm -> Exp
list s = case s of
    Con x -> repeat_ `App` x
    Itr x y -> iterate_ `App` x`App` y
    Map x y -> map_ `App` x `App` list y
    Zip x y z -> zipWith_ `App` x `App` list y `App` list z
    Out p x -> writeFile_ `App` Str p `App` list x
