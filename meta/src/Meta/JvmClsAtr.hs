-- | Java class file attributes.
module Meta.JvmClsAtr (
    Attribute
    , make
) where

import qualified Meta.JvmAccess as A

{- |
Ontological kitchen sink.

An attribute may be a 'Code', a debugging information mapping source code to bytecode, or something else.
There are more than ten kinds of attributes.
-}
data Attribute name content = Mk_attribute {
        a_name :: name
        , a_content :: content
    } deriving (Read, Show, Eq)

-- | An inhabitant.
make :: name -> content -> Attribute name content
make = Mk_attribute

instance A.Get_name (Attribute name content) name where
    get_name = a_name

instance A.Get_content (Attribute name content) content where
    get_content = a_content
