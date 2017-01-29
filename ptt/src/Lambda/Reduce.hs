module Lambda.Reduce
(
    -- * Normal-order beta reduction

    reduce
)
where

import qualified Control.Applicative as A

import Control.Monad
    (
        MonadPlus
        , guard
    )

import qualified Data.Maybe as DM

import Lambda.Deconstruct
import Lambda.Term

{- |
Normal-order beta reduction (capture-avoiding substitution).

If the input term is not a beta redex, this produces 'A.empty'.

A beta redex is an @App x y@ where @x@ is a lambda abstraction or a beta redex.

Note that there is a beta redex that reduces to itself
such as @(\\ x -> x x) (\\ x -> x x)@.

Redex is an acronym for /red/ucible /ex/pression.
-}
reduce :: (MonadPlus m, Term t) => t -> m t
reduce t =
    app (lam pure pure) pure t |> (\ ((name, body), arg) -> substitute name arg body)
    <|> (app pure pure t >>= \ (fun, arg) -> mkApp <$> reduce fun <*> pure arg)

-- | Capture-avoiding substitution.
substitute :: (Term t) => String -> t -> t -> t
substitute name replacement = subst
    where
        subst term =
            varNamed name term |> const replacement
            <|> app pure pure term |> (\ (fun, arg) -> mkApp (subst fun) (subst arg))
            <|> lamNotNamed name term |> (\ (n, body) -> mkLam n (subst body))
            <!> term
        varNamed name_ t = do
            n <- var t
            guard (n == name_)
            pure ()
        lamNotNamed name_ t = do
            (n, body) <- lam pure pure t
            guard (n /= name_)
            pure (n, body)
