-- | Internal. Do not use. Use "Meta.WrapM".
module Meta.Wrap where

-- * Things

-- | Indentation if at beginning of line.
type IndBol = Int

type Length = Int

-- | Space if previous character is not a whitespace (space or line break).
space :: Thing
space = Space

-- | Unsplittable text.
atom :: IndBol -> String -> Thing
atom ind str = Atom ind str

-- | Forced line break.
break :: Thing
break = Break

-- * Layout

defLayOpt :: LayOpt
defLayOpt = MkLayOpt {
        loLineWidth = 120
    }

layout :: LayOpt -> [Thing] -> String
layout _ [] = []
layout lo things = unlines $ go stInit "" "" things
    where
        go :: State -> String -> String -> [Thing] -> [String]
        -- 'pending' is spaces not yet committed.
        -- 'output' is things already committed.
        go _st _pending output [] = [output]
        go st pending output ths@(h : t) =
            case h of
                Atom ind str ->
                    let
                        indent = if isEmpty then replicate ind ' ' else ""
                        newOutput = output ++ pending ++ indent ++ str
                        hasRoom = length newOutput <= maxWidth
                        shouldBeHere = isEmpty || hasRoom
                        here = go stBlack "" newOutput t
                        there = output : go stWhite "" "" ths
                    in
                        if shouldBeHere then here else there
                Space ->
                    if prevCharWasWhite then skip else maybeAddSpace
                Break ->
                    output : go stWhite "" "" t
            where
                maxWidth = loLineWidth lo
                isEmpty = null output
                skip = go st pending output t
                maybeAddSpace = go stWhite " " output t
                prevCharWasWhite = sPrevCharWasWhite st
                stWhite = st { sPrevCharWasWhite = True }
                stBlack = st { sPrevCharWasWhite = False }

data State
    = MkState {
        sIndent :: Int
        , sPrevCharWasWhite :: Bool
    } deriving (Show, Read)

stInit :: State
stInit = MkState 0 True

-- * Internal

data LayOpt
    -- | Internal. Do not use. Use 'defLayOpt'.
    = MkLayOpt {
        loLineWidth :: Int -- ^ desired maximum line width
    } deriving (Show, Read)

data Thing
    = Atom IndBol String
    | Space
    | Break
    deriving (Show, Read)
