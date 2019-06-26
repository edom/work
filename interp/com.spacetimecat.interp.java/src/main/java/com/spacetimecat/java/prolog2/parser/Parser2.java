package com.spacetimecat.java.prolog2.parser;

import java.util.function.IntFunction;

/*

A recognizer is a function String -> Boolean.

A parser is a function String -> Tree.

*/

/**
 * Parsing, incremental, fixed point.
 */
class Parser2 {

    static class Parser {
        boolean error;
        Cons in;
        IntFunction<Parser> next;
    }

}
