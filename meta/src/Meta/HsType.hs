module Meta.HsType where

-- * Name

-- | VarName or ConName.
type Name = String

-- | Example: @\"MyModule.MyType\"@. Dot-separated components. Each component begins with uppercase letter.
type QualName = String

-- | Example: @\"myVar\"@. Must begin with lowercase letter.
type VarName = String

-- | Example: @\"MyCon\"@. Must begin with uppercase letter.
type ConName = String

type TypName = ConName

type ClsName = ConName

type ModName = QualName

-- * Symbol

-- | Symbol with provenance information. Qualified symbol.
data Sym
    = MkSym {
        sMod :: ModName
        , sName :: Name
    } deriving (Eq, Ord, Read, Show)

sQualName :: Sym -> QualName
sQualName s = sMod s ++ "." ++ sName s

-- | Symbol referring to a class.
type SymCls = Sym

-- | Symbol referring to a constructor.
type SymCon = Sym

-- | Symbol referring to a Haskell type (whose kind is @*@).
type SymTyp = Sym

-- ** Predefined symbols

sc_Read :: SymCls
sc_Read = MkSym "Prelude" "Read"

sc_Show :: SymCls
sc_Show = MkSym "Prelude" "Show"

-- * Type

{- |
Lambda calculus at universe 1.
-}
data Type
    = Val SymTyp -- ^ @Val \"A\"@ represents @A@
    | Var VarName -- ^ @Var \"a\"@ represents @a@
    | App Type Type -- ^ If @a@ represents @x@ and @b@ represents @y@, then @App a b@ represents @x y@.
    | Lam VarName Type -- ^ If @b@ represents @y@, then @Lam \"a\" b@ represents @forall a. y@
    | Arr Type Type -- @a -> b@
    deriving (Read, Show)

-- | Get used 'VarName's.
getVars :: Type -> [VarName]
getVars typ = case typ of
    Val _ -> []
    Var n -> [n]
    App a b -> getVars a ++ getVars b
    Lam n t -> n : getVars t
    Arr a b -> getVars a ++ getVars b

getSyms :: Type -> [Sym]
getSyms t = case t of
    Val s -> [s]
    Var _ -> []
    App a b -> getSyms a ++ getSyms b
    Lam _ a -> getSyms a
    Arr a b -> getSyms a ++ getSyms b

mkVal :: String -> String -> Type
mkVal modu nam = Val $ MkSym modu nam

-- * Predefined types

string :: Type
string = mkVal "Prelude" "String"

int32 :: Type
int32 = mkVal "Data.Int" "Int32"

int64 :: Type
int64 = mkVal "Data.Int" "Int64"

-- * Experimental

-- | Not implemented: Transform @a -> ... -> z@ to @(Monad m) => m (a -> ... -> z)@
pureM :: Type -> Type
pureM = error "pureM"

-- | Not implemented: Transform @a -> ... -> z@ to @(Monad m) => m a -> ... -> m z@
liftM :: Type -> Type
liftM = error "liftM"
