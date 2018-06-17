module Meta.WrapM where

import qualified Meta.Wrap as W

-- * Constructors

-- | Do nothing.
nop :: Prog ()
nop = pure ()

-- | Insert an unsplittable text.
atom :: String -> Prog ()
atom str = mapState $ saAtom str

-- | Insert a space if necessary.
space :: Prog ()
space = mapState saSpace

-- | Force a line break.
break :: Prog ()
break = mapState saBreak

-- | @indented m@ is @m@ with increased indentation.
indented :: Prog a -> Prog a
indented (MkProgM k) = MkProgM $ \ s0 ->
    let
        (s1, a) = k s0 { sCurInd = sCurInd s0 + sIndSize s0 }
    in
        (s1 { sCurInd = sCurInd s0 }, a)

commaSep :: [Prog ()] -> Prog ()
commaSep = sepBy (atom "," >> space)

sepBy :: Prog () -> [Prog ()] -> Prog ()
sepBy _ [] = nop
sepBy _ [x] = x
sepBy sep (h : t) = h >> sep >> sepBy sep t

-- * Rendering

render :: Prog a -> String
render = run defState

run :: State -> Prog a -> String
run st0 (MkProgM k) = W.layout W.defLayOpt things
    where
        finalState = fst $ k st0
        things = sThings finalState

-- * Internal

-- ** SA (state action)

saAtom :: String -> State -> State
saAtom str s = saAddThing (W.atom (sCurInd s) str) s

saSpace :: State -> State
saSpace s = saAddThing W.space s

saBreak :: State -> State
saBreak s = saAddThing W.break s

saAddThing :: W.Thing -> State -> State
saAddThing th s = s { sThings = sThings s ++ [th] }

-- ** State

data State = MkState {
        sIndSize :: Int
        , sMaxLineWidth :: Int
        , sThings :: [W.Thing]
        , sCurInd :: Int
    } deriving (Show, Read)

defState :: State
defState = MkState {
        sIndSize = 4
        , sMaxLineWidth = 120
        , sThings = []
        , sCurInd = 0
    }

mapState :: (State -> State) -> Prog ()
mapState f = MkProgM $ \ s -> (f s, ())

get :: (State -> a) -> Prog a
get f = MkProgM $ \ s -> (s, f s)

-- ** Prog Monad instance

newtype Prog a = MkProgM {
        unProgM :: State -> (State, a)
    }

mapSnd :: (b -> c) -> (a, b) -> (a, c)
mapSnd f (a, b) = (a, f b)

instance Functor Prog where
    fmap f (MkProgM k) = MkProgM $ \ s -> mapSnd f (k s)

instance Applicative Prog where
    pure x = MkProgM $ \ s -> (s, x)
    (<*>) (MkProgM kf) (MkProgM kx) = MkProgM $ \ s0 ->
        let
            (s1, f) = kf s0
            (s2, x) = kx s1
        in
            (s2, f x)

instance Monad Prog where
    return = pure
    (>>=) (MkProgM kx) k = MkProgM $ \ s0 ->
        let
            (s1, x) = kx s0
            MkProgM j = k x
        in
            j s1
