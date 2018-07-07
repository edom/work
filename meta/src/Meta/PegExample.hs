module Meta.PegExample where

import Prelude ()
import Meta.PreludeGrammar

import Meta.Peg

-- We should lex before parse, to obviate the "white" noise below.
exampleParse :: Value Char
exampleParse = parse cdecl "unsigned int** foo (signed int bar, signed char qux, unsigned long** qux);"
    where
        -- Problem:
        -- C parsing depends on the symbol table.
        -- We can't parse "unsigned int32_t" without knowing whether "int32_t" has been typedefed.
        cdecl = typ >! name >? parl >? params >? parr >? scol
        typ = (name / kvoid / typ_char / typ_short / typ_int / typ_long) > stars
        stars = (opt white > star > stars) / empty
        typ_char = opt (ksig > white) > kchar
        typ_short = opt (ksig > white) > kshort > opt (white > kint)
        typ_int = (opt (ksig > white) > kint) / (ksig > and (white > name))
        typ_long = opt (ksig > white) > klong > opt (white > kint)
        kvoid = string "void"
        kchar = string "char"
        kshort = string "short"
        kint = string "int"
        klong = string "long"
        ksig = string "unsigned" / string "signed"
        kw = kvoid / kchar / kshort / kint / klong / ksig

        param = typ >! name
        params = opt (param >? many0 (comma >? param))
        letter = any $ term <$> ['A'..'Z'] ++ ['a'..'z'] ++ ['_']
        digit = any $ term <$> ['0'..'9']
        name = not (kw > not name_tail_char) > letter > many0 name_tail_char
        name_tail_char = letter / digit
        star = term '*'
        comma = term ','
        parl = term '('
        parr = term ')'
        scol = term ';'
