module Error_internal where

import qualified Control.Exception as Ex
import qualified Data.Typeable as Ty

{- |
'Exc' enables us to 'throw' 'String's.

Do not use this directly.
-}
newtype Exc
    = Mk_exc { un_exc :: String }
    deriving (Ty.Typeable, Read, Show)

instance Ex.Exception Exc
