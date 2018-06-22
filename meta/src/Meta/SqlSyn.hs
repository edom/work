{- |
We should abandon this? Just generate a 'String'?
-}
module Meta.SqlSyn where

import Prelude hiding (exp, (+))

-- * Select

data Select a
    = MkSelect {
        _sCols :: [Col a]
        , _sFrom :: From a
        , _sWhere :: Maybe (Exp a)
        , _sOrder :: Order a
        , _sLimit :: Maybe Integer
        , _sOffset :: Maybe Integer
    } deriving (Read, Show)

-- * Column

type Name = String

type Alias = String

data Col a
    = MkCol {
        _cExp :: Exp a
        , _cAlias :: Maybe Alias
    } deriving (Read, Show)

data From a
    = FPure a
    | FTable Name
    | FInnerJoin (From a) (From a)
    | FLeftJoin (From a) (From a)
    | FRightJoin (From a) (From a)
    | FOuterJoin (From a) (From a)
    deriving (Read, Show)

data Exp a
    = EPure a
    | EBool Bool
    | EInt Int
    | EStr String
    | EName Name
    | EApp Name [Exp a]
    | EEq (Exp a) (Exp a)
    deriving (Read, Show)

data Order a
    = ONone
    | OAsc (Col a) (Order a)
    | ODesc (Col a) (Order a)
    deriving (Read, Show)

-- * Render

data Render a
    = RPure a
    | REmpty
    | RSeq (Render a) (Render a)
    | RKeyword String
    | RLiteral String
    deriving (Read, Show)

render_select :: Select a -> Render a
render_select sel =
    RKeyword "SELECT" + s_cols + s_from
    where
        (+) = RSeq
        s_cols = rconcat $ map renderCol (_sCols sel)
        renderCol col =
            case _cAlias col of
                Nothing -> s_exp
                Just alias -> s_exp + RKeyword "AS" + RLiteral alias
            where
                s_exp = render_exp (_cExp col)
        s_from = RKeyword "FROM" + REmpty -- FIXME

render_exp :: Exp a -> Render a
render_exp = recur
    where
        recur exp = case exp of
            EPure a -> RPure a
            EBool a -> case a of
                False -> RKeyword "FALSE"
                True -> RKeyword "TRUE"
            EInt n -> RLiteral (show n)
            EStr s -> RLiteral ("'" ++ escape_str s ++ "'")
            EName n -> RLiteral n
            EApp fun args -> RLiteral (fun ++ "(") + rconcat (map recur args) + RLiteral ")"
            EEq a b -> recur a + RLiteral "=" + recur b
        (+) = RSeq

rconcat :: [Render a] -> Render a
rconcat [] = REmpty
rconcat (h : t) = h `RSeq` rconcat t

-- FIXME
escape_str :: String -> String
escape_str x = x

-- FIXME
escape_name :: String -> String
escape_name x = x
