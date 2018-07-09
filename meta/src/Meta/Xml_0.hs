{-# LANGUAGE Rank2Types #-}

module Meta.Xml_0 (
    Module_Meta_Xml(..)
    , module_Meta_Xml
) where

import qualified Meta.Xml as X

data Module_Meta_Xml = Module_Meta_Xml {
        elm :: forall a. (X.CElm a) => X.Name -> [X.Atr] -> [X.Node] -> a
        , atr :: X.Name -> X.Value -> X.Atr
        , nAtr :: X.Ns -> X.Name -> X.Value -> X.Atr
        , proc :: forall a. (X.CProc a) => X.Name -> [X.Atr] -> a
        , text :: String -> X.Node
    }

module_Meta_Xml :: Module_Meta_Xml
module_Meta_Xml = Module_Meta_Xml {
        elm = X.elm
        , atr = X.atr
        , nAtr = X.nAtr
        , proc = X.proc
        , text = X.text
    }
