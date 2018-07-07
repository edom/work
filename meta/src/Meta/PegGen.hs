{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- |
Generate a parser from PEG description.
-}
module Meta.PegGen (
    -- * Parsing
    parse
    , Result
    , Error
    -- * Grammar
    , R.Grammar
    , R.grammar
    -- * Rule
    , R.Module_Meta_PegGenRule(..)
    , R.module_Meta_PegGenRule
    -- * Concrete syntax tree with location
    , Tree
    , pretty
) where

import Prelude ()
import Meta.PreludeGrammar

import qualified Meta.PegGenLoc as L
import qualified Meta.PegGenRule as R
import qualified Meta.PegGenTree as T

import qualified Text.PrettyPrint as P

type Error = String

data Tree t
    = MkTree L.Loc (T.Tree t (Tree t))
    deriving (Read, Show)

{- |
Pretty print the tree.
-}
pretty :: (Show t) => Tree t -> String
pretty = P.render . doc
    where
        doc input@(MkTree loc tree) = case tree of
            T.Nil -> P.text "Empty" P.<+> tloc
            T.Term t -> P.text "Terminal" P.<+> P.text (show t) P.<+> tloc
            T.Seq a b -> (P.text "Sequence" P.<+> tloc) P.$$ P.nest indent (doc a P.$$ doc b)
            T.Call name tr -> (P.text "Call" P.<+> P.doubleQuotes (P.text name) P.<+> tloc) P.$$ P.nest indent (doc tr)
            _ -> error $ "Meta.PegGen.pretty: Not implemented: " ++ show input
            where
                tloc = P.parens $ P.text $ L.display_short loc
                indent = 2

type Result t = Either Error (Tree t, L.Loc, [t])

parse :: forall t. (Eq t, Show t) => R.Grammar t -> [t] -> Result t
parse = pars L.begin
    where
        pars :: L.Loc -> R.Grammar t -> [t] -> Result t
        pars loc_ gram tokens_ = do
            start <- R.find_start gram
            (n,p,t) <- go start loc_ tokens_
            return (MkTree loc_ (T.Call rule_name n), p, t)
            where
                go :: R.Exp t -> L.Loc -> [t] -> Result t
                go exp loc tokens = case exp of

                    R.Empty ->
                        Right (mktree T.Nil, loc, tokens)

                    R.Term t ->
                        if take 1 tokens == [t]
                            then Right (mktree (T.Term t), L.char loc, drop 1 tokens)
                            else err ("Expecting " ++ show t)

                    R.Seq a b -> do
                        let p0 = loc
                            t0 = tokens
                        (n1,p1,t1) <- go a p0 t0
                        (n2,p2,t2) <- go b p1 t1
                        return (mktree (T.Seq n1 n2), p2, t2)

                    R.Or a b -> case go a loc tokens of
                        Right x -> Right x
                        Left _ -> go b loc tokens

                    R.End -> case tokens of
                        [] -> Right (mktree T.Nil, loc, tokens)
                        _ -> err "Expecting end of input"

                    R.Call name ->
                        pars loc (R.set_start name gram) tokens

                    R.Many subexp ->
                        case go subexp loc tokens of
                            Right ((MkTree _ T.Nil),_,_) -> err $ "Infinite loop detected. Please inform the programmer who wrote the parser."
                                ++ " Technical details: A child of Many must consume some input."
                            Right (n,p,t) -> case go exp p t of
                                Right (nn,pp,tt) -> Right (mktree (T.Seq n nn), pp, tt)
                                Left e -> err ("The impossible happened: Match failure in Many: " ++ e)
                            _ -> go R.Empty loc tokens

                    _ ->
                        error ("Meta.PegGen.parse: Not implemented: " ++ show exp)

                    where
                        err msg = Left $ "Parse error at " ++ L.display loc ++ " while applying rule \"" ++ rule_name ++ "\": " ++ msg
                        mktree = MkTree loc
                rule_name = R.get_start_name gram
