{-# LANGUAGE RankNTypes #-}

module Meta.Xml where

import qualified Data.String as S

-- * Type

type Ns = String

type Name = String

type Value = String

-- * Document

mkDoc :: [Node] -> Doc
mkDoc = MkDoc

data Doc
    -- | Internal. Do not use. Use 'mkDoc'.
    = MkDoc {
        dNodes :: [Node]
    } deriving (Read, Show)

-- * Node

text :: String -> Node
text = NText

data Node
    = NText String -- ^ Text. Internal. Do not use. Use 'text'.
    | NElm Elm -- ^ Element. Internal. Do not use. Use 'elm' or 'nElm'.
    | NProc Proc -- ^ Processing instruction. Internal. Do not use. Use 'proc'.
    deriving (Read, Show)

class ToNode a where toNode :: a -> Node
instance ToNode String where toNode = NText
instance ToNode Elm where toNode = NElm
instance S.IsString Node where fromString = NText

-- * Element

class CElm a where

    nElm :: Ns -> Name -> [Atr] -> [Node] -> a

    elm :: Name -> [Atr] -> [Node] -> a
    elm = nElm ""

instance CElm Elm where nElm = MkElm
instance CElm Node where nElm ns nam as cs = NElm (MkElm ns nam as cs)

data Elm
    -- | Internal. Do not use. Use 'elm' or 'nElm'.
    = MkElm {
        eNs :: Ns
        , eName :: Name
        , eAtrs :: [Atr]
        , eChildren :: [Node]
    } deriving (Read, Show)

-- * Processing instruction

class CProc a where proc :: Name -> [Atr] -> a
instance CProc Proc where proc = MkProc
instance CProc Node where proc nam atrs = NProc (MkProc nam atrs)

data Proc
    = MkProc {
        pName :: Name
        , pAtrs :: [Atr]
    } deriving (Read, Show)

-- * Attribute

atr :: Name -> Value -> Atr
atr = MkAtr ""

nAtr :: Ns -> Name -> Value -> Atr
nAtr = MkAtr

data Atr
    -- | Internal. Do not use. Use 'atr' or 'nAtr'.
    = MkAtr {
        aNs :: Ns
        , aName :: Name
        , aValue :: Value
    } deriving (Read, Show)

-- * Render

renderDoc :: RenOpt -> Doc -> String
renderDoc ro (MkDoc nodes) = concatMap (renderNode ro) nodes

defRenOpt :: RenOpt
defRenOpt = MkRenOpt {
        roIndent = 4
        , roCurInd = 0
    }

-- | Rendering options.
data RenOpt
    -- | Internal. Do not use. Use 'defRenOpt'.
    = MkRenOpt {
        roIndent :: Int -- ^ Indent size.
        , roCurInd :: Int -- ^ Internal. Do not use. Current indent level.
    } deriving (Read, Show)

-- * Internal

-- | Internal. Do not use.
enter :: RenOpt -> RenOpt
enter ro = ro { roCurInd = roCurInd ro + roIndent ro }

indent :: RenOpt -> String
indent ro = replicate (roCurInd ro) ' '

renderNode :: RenOpt -> Node -> String
renderNode ro nod = case nod of
    NText s -> escape s
    NElm e -> renderElm ro e
    NProc p -> renderProc ro p

renderAtr :: RenOpt -> Atr -> String
renderAtr ro (MkAtr ns nam val) =
    "\n" ++ indent ro ++ qual ns nam ++ "=\"" ++ escape val ++ "\""

-- | Internal. Do not use.
renderAtrs :: RenOpt -> [Atr] -> String
renderAtrs ro atrs = concatMap (renderAtr ro) atrs

renderElm :: RenOpt -> Elm -> String
renderElm ro el@(MkElm ns nam atrs chds) =
    if preserveSpace
        then shortForm
        else longForm
    where
        shortForm =
            openTag
            ++ concatMap (renderNode roChd) chds
            ++ closeTag
        longForm =
            openTag ++ "\n"
            ++ unlines (map (\ chd -> indent roChd ++ renderNode roChd chd) chds)
            ++ indent ro ++ closeTag
        openTag = "<" ++ qname ++ sAtrs ++ ">"
        closeTag = "</" ++ qname ++ ">"
        qname = qual ns nam
        hasAtr = not $ null atrs
        sAtrs = if hasAtr
            then " " ++ renderAtrs roChd atrs
            else ""
        hasChild = not $ null chds
        preserveSpace = eHasTextChild el
        roChd = enter ro

        eHasTextChild :: Elm -> Bool
        eHasTextChild el = any isTextNode $ eChildren el

        isTextNode :: Node -> Bool
        isTextNode nod = case nod of
            NText _ -> True
            _ -> False

renderProc :: RenOpt -> Proc -> String
renderProc ro (MkProc nam atrs) =
    "<?" ++ nam ++ " " ++ renderAtrs ro atrs ++ "?>\n"

-- | Internal. Do not use.
escape :: Value -> String
escape val = concatMap esc val
    where
        esc c = case c of
            '"' -> "&quot;"
            '&' -> "&amp;"
            '<' -> "&lt;"
            '>' -> "&gt;"
            _ -> [c]

-- | Internal. Do not use.
qual :: Ns -> Name -> String
qual ns nam = case ns of
    "" -> nam
    _ -> ns ++ ":" ++ nam
