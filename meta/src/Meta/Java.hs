module Meta.Java where

data Type
    = TInt32
    | TInt64
    | TRef Class
    deriving (Read, Show)

data Annot
    = MkAnnot Type
    deriving (Read, Show)

data Class
    = MkClass {
        cPkg:: String
        , cName :: String
        , cMembers :: [Member]
        , cAnnots :: [Annot]
        , cComment :: String
        , cExternal :: Bool
    } deriving (Read, Show)

data Param
    = MkParam {
        pType :: Type
        , pName :: String
        , pAnnots :: [Annot]
    } deriving (Read, Show)

data TypeParam
    = MkTypeParam {
        tName :: String
    } deriving (Read, Show)

data Field
    = MkField {
        fType :: Type
        , fName :: String
        , fAnnots :: [Annot]
        , fComment :: String
    } deriving (Read, Show)

data Method
    = MkMethod {
        mRet :: Type
        , mName :: String
        , mTypeParams :: [TypeParam]
        , mParams :: [Param]
        , mBody :: [Stmt]
        , mAnnots :: [Annot]
        , mComment :: String
    } deriving (Read, Show)

data Stmt
    = SExp Expr
    | SRet Expr
    | SThrow Expr
    | SBlock [Stmt]
    deriving (Read, Show)

data Expr = EInt Integer | EDbl Double | EStr String | EPlus Expr Expr
    | ECall Expr Method [Expr]
    | EAssign LVal RVal
    | SNew Class
    deriving (Read, Show)

data LVal = MkLVal deriving (Read, Show)

data RVal = MkRVal deriving (Read, Show)

data Member
    = MField Field
    | MMethod Method
    | MInit [Stmt]
    | MStaticInit [Stmt]
    | MComment String
    deriving (Read, Show)
