{-# LANGUAGE Haskell98 #-}
{- |
Haskell 98 command line argument parsing library.
-}
module Option
(
    -- * Usage
    -- $usage
    -- * Running
    help
    , parse
    , parse_io
    -- * Lifting
    , lift
    -- * Grammar
    , Parse
    , end
    , exact
    , option
    , var
    , read
    , (<|>)
    , many
    , many_
    , true
    , false
    -- * State
    , modify
)
where

import qualified Control.Applicative as A
import qualified System.IO.Error as Ie
import qualified Text.Read as Tr

import qualified Control.Monad.Trans.Writer as W

import qualified Text.Parsec as P

import Prelude hiding (read)

{- $usage
@
import qualified "System.Environment" as Env

import Option

data Cnf
    = Mk_cnf
    {
        c_quiet :: 'Bool'
        , c_file :: 'FilePath'
        , c_count :: 'Int'
    }
    deriving ('Read', 'Show')

c_new :: Cnf
c_new = Mk_cnf False \"\" 0

main :: 'IO' ()
main = do
    args <- Env.'System.Environment.getArgs'
    cnf <- 'parse_io' args c_new grammar
    -- do whatever you want with cnf
    -- ...
    'print' cnf
    where
        grammar =
            'many_'
            (
                'option' \"-q\" ('true' '>>=' 'modify' '.' set_quiet)
                    \"print less messages\"
                '<|>' 'option' \"-f\" ('var' \"FILE\" '>>=' 'modify' '.' set_file)
                    \"input file path\"
                '<|>' 'option' \"-n\" ('var' \"COUNT\" '>>=' Option.'read' '>>=' 'modify' '.' set_count)
                    \"number of times to perform operation\"
            )
            '<|>' 'end'
            where
                set_quiet x c = c { c_quiet = x }
                set_file x c = c { c_file = x }
                set_count x c = c { c_count = x }
@
-}

{- |
Generate the help message for the argument list in the grammar.
-}
help :: Parse c a -> String
help p = unlines $ zipWith (\ x y -> pad left_width x ++ "    " ++ y) left right
    where
        descs = p_help p
        left = map (unwords . o_args) descs
        right = map o_usage descs
        left_width = maximum (map length left)
        pad :: Int -> String -> String
        pad colw str = take (max colw (length str)) (str ++ repeat ' ')

{- |
Parse the argument list into the configuration.
-}
parse
    :: [String] -- ^ argument list
    -> c -- ^ initial configuration
    -> Parse c a -- ^ grammar
    -> Either String c

parse arglist cnf grammar =
    either Left (\ ((_, c), _) -> Right c) $ p_run grammar (arglist, cnf)

-- | This is like 'parse' but throws an 'Ie.IOError' instead of returning 'Left'.
parse_io :: [String] -> c -> Parse c a -> IO c
parse_io arglist cnf grammar =
    either (Ie.ioError . Ie.userError) return (parse arglist cnf grammar)

{- |
@
lift = 'either' 'fail' 'pure'
@
-}
lift :: Either String a -> Parse c a
lift = either fail pure

-- | This matches the end of argument list.
end :: Parse c ()
end = p_new { p_run = \ s -> case s of
    s@([], c) -> Right (s, ())
    _ -> Left "match fail: expecting end of arguments"
}

-- | This matches the string exactly.
exact :: String -> Parse c ()
exact seeked = p_new { p_run = \ (s, c) -> case s of
    [] -> Left "match fail: exact: end of input"
    x : y | x == seeked -> Right ((y, c), ())
    _ -> Left $ "match fail: exact: expecting " ++ seeked
}

-- | @option header body usage@ matches @header@ exactly, and then matches @body@.
option :: String -> Parse c a -> String -> Parse c a
option header body usage =
    (exact header *> body)
    { p_help = [o_new { o_args = header : p_args body, o_usage = usage }]
    , p_args = [] }

{- |
This matches any argument and gives it the name.

The name is usually all-uppercase.
-}
var :: String -> Parse c String
var name =
    p_new
    {
        p_args = [name]
        , p_run = \ s -> case s of
            (x : y, c) -> Right ((y, c), x)
            _ -> Left $ "match fail: var: expecting " ++ name
    }

{- |
This is 'lift'ed 'Tr.readEither'.
-}
read :: (Read a) => String -> Parse c a
read = lift . Tr.readEither

{- |
@x \<|\> y@ tries @x@, and tries @y@ if @x@ fails.

This is a specialization of 'A.<|>' of 'A.Alternative' in "Control.Applicative".
-}
(<|>) :: Parse c a -> Parse c a -> Parse c a
(<|>) = (A.<|>)
infixl 3 <|>

{- |
@many x@ tries @x@ until it fails.

This is a specialization of 'A.many' of 'A.Alternative' in "Control.Applicative".
-}
many :: Parse c a -> Parse c [a]
many = A.many

-- | This is like 'many' but this discards the results.
many_ :: Parse c a -> Parse c ()
many_ x = A.many x *> pure ()

-- | @true = 'pure' 'True'@
true :: Parse c Bool
true = pure True

-- | @false = 'pure' 'False'@
false :: Parse c Bool
false = pure False

-- | Apply the function to the configuration that is being built.

modify :: (c -> c) -> Parse c ()
modify f = p_new { p_run = \ (s, c) -> Right ((s, f c), ()) }

get_c :: Parse c c
get_c = p_new { p_run = \ (s, c) -> Right ((s, c), c) }

-- * Test

data Cnf
    = Mk_cnf
    {
        c_help :: Bool
        , c_input :: FilePath
        , c_pkgs :: [String]
    }
    deriving (Read, Show)

optest = do
    putStrLn $ help parse
    case p_run parse (arglist, initcnf) of
        Left e -> putStrLn e
        Right x -> print x
    where
        initcnf = Mk_cnf False "" []
        arglist = ["-h", "-b", "content", "--", "pkg", "wht", "ooh"]
        -- arglist = ["-i", "foobar"]
        parse =
            many
                (
                option "-h" (modify (set_h True)) "show help"
                <|> option "--help" (modify (set_h True)) "show help"
                <|> option "-s" (var "FILE" >>= modify . set_fn) "set sources.list file path"
                -- <|> option "-b" (var "FILE" >>= modify . set_fn) "set mirror base directory"
                <|> option "-b" (var "FILE" *> pure ()) "set mirror base directory"
                <|> option "-c" (var "SRC" *> var "DST" *> pure ()) "copy"
                )
            *> exact "--"
            *> many (var "PKG" >>= modify . add_pkg)
            *> end
        set_h b c = c { c_help = b }
        set_fn s c = c { c_input = s }
        add_pkg s c = c { c_pkgs = s : c_pkgs c }

-- * Internal

{- |
Describes an option.
-}
data Optdesc
    = Mk_optdesc
    {
        o_args :: [String]
        , o_usage :: String
    }

o_new :: Optdesc
o_new = Mk_optdesc [] ""

{- |
[@c@] The configuration type that is being built.
-}
data Parse c a
    = Mk_parse
    {
        p_help :: [Optdesc]
        , p_args :: [String]
        , p_run :: ([String], c) -> Either String (([String], c), a)
    }

p_new :: Parse c ()
p_new = Mk_parse [] [] $ \ s -> Right (s, ())

next_token :: Parse c String
next_token = p_new { p_run = \ (arglist, c) -> case arglist of
    [] -> Left "next_token: unexpected end of argument list"
    x:y -> Right ((y, c), x)
    }

instance Functor (Parse c) where
    fmap f p = p
        {
            p_run = \ s -> case p_run p s of
                Left e -> Left e
                Right (t, a) -> Right (t, f a)
        }

instance Applicative (Parse c) where
    pure x = p_new { p_run = \ s -> Right (s, x) }
    (<*>) p q = p_new
        {
            p_help = p_help p ++ p_help q
            , p_args = p_args p ++ p_args q
            , p_run = \ s_0 -> do
                (s_1, f) <- p_run p s_0
                (s_2, x) <- p_run q s_1
                return (s_2, f x)
        }

instance A.Alternative (Parse c) where
    empty = p_new { p_run = const $ Left "empty" }
    (<|>) p q = p_new { p_help = h, p_run = r }
        where
            h = p_help p ++ p_help q
            r s = case p_run p s of
                Left e -> case p_run q s of
                    Left _ -> Left e
                    x -> x
                x -> x
    many m =
        loop
        where
            loop =
                (do
                    x <- m
                    y <- loop
                    return (x : y)
                )
                <|> pure []

instance Monad (Parse c) where
    return = pure
    fail s = p_new { p_run = const $ Left s }
    (>>=) p k = p { p_run = \ s_0 -> do
        (s_1, a) <- p_run p s_0
        p_run (k a) s_1
        }
    (>>) = (*>)
