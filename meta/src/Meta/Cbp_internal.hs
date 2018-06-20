module Meta.Cbp_internal where

data Type
    = TUnit -- ^ unit type
    | TBool -- ^ boolean type
    | TNat8 -- ^ unsigned 8-bit integer
    | TInt8 -- ^ two's-complement 8-bit signed integer
    | TInt32 -- ^ two's-complement 32-bit integer
    | TInt64 -- ^ two's-complement 64-bit integer
    | TString -- ^ string (character sequence)
    | TDecimal -- ^ arbitrary precision decimal (such as java.math.BigDecimal)
    | TArray Type Int -- ^ fixed-length array
    | TClass Class -- ^ aggregate
    | TPointer Type -- ^ pointer
    | TNullablePtr Type -- ^ nullable pointer
    deriving (Read, Show)

data Class
    -- | Internal. Do not use. Use 'c_empty'.
    = MkClass {
        cPkg :: String -- ^ package
        , cName :: Class_name -- ^ simple name (name without package)
        , cParents :: [Class]
        , cMembers :: [Member]
        , cAbstract :: Bool
    } deriving (Read, Show)

-- | Simple name, without package.
type Class_name = String

c_empty :: Class
c_empty = MkClass {
        cPkg = ""
        , cName = ""
        , cParents = []
        , cMembers = []
        , cAbstract = False
    }

data Member
    = MField Field
    | MMethod Method
    | MProp Prop
    | MLineComment String
    | MBlockComment String
    deriving (Read, Show)

data Field = MkField {
        fType :: Type
        , fName :: String
    } deriving (Read, Show)

data Method
    -- | Internal. Do not use. Use 'defMethod'.
    = MkMethod {
        mRet :: Type
        , mName :: String
        , mParams :: [Param]
        , mStatic :: Bool -- ^ true means has no implicit first parameter
        , mAbstract :: Bool -- ^ true means has no body
        , mVirtual :: Bool -- ^ true means overrideable; false means final
        , mOverride :: Bool -- ^ true means this method is expected to override an overrideable method in a parent
        , mBody :: [Sta]
    } deriving (Read, Show)

defMethod :: Method
defMethod = MkMethod {
        mRet = TUnit
        , mName = ""
        , mParams = []
        , mStatic = False
        , mAbstract = False
        , mVirtual = False
        , mOverride = False
        , mBody = []
    }

data Prop
    = MkProp {
        prType :: Type -- ^ Internal. Do not use. Use 'P.getType' and 'P.setType'.
        , prName :: String -- ^ Internal. Do not use. Use 'P.getName' and 'P.setName'.
    } deriving (Read, Show)

data Param = MkParam {
        pType :: Type
        , pName :: String
    } deriving (Read, Show)

-- * Statement

data Sta
    = SExp Exp -- ^ expression statement
    | SRet Exp -- ^ return statement
    | SDecl Type String -- ^ variable declaration statement
    | SAssign String Exp -- ^ assignment statement
    deriving (Read, Show)

-- * Expression

-- | Embedded lambda calculus.

data Exp
    = EConUnit
    | EConInt Integer
    | EConStr String
    | EConDbl Double
    | EPlus Exp Exp
    | EIf Exp Exp Exp -- ^ EIf condition truePart falsePart
    | EDot Exp Exp -- ^ dot (member access) expression
    | ECall Exp [Exp] -- ^ call expression
    deriving (Read, Show)
