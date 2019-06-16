package com.spacetimecat.java.prolog2.parser;

import static com.spacetimecat.java.prolog2.parser.Either.left;
import static com.spacetimecat.java.prolog2.parser.Either.right;

import java.io.Reader;
import java.nio.IntBuffer;

public final class Parser {

    Cons input;

    Parser (Cons input) {
        this.input = input;
    }

    public static Parser create (String file, Reader in) {
        return new Parser(Cons.of(file, in));
    }

    static final class Error {

        final String message;

        public Error (String message) {
            this.message = message;
        }

    }

    // -------------------- recognizer

    @FunctionalInterface
    interface Recognizer<A> {
        Either<Error, A> call ();
    }

    // read nothing if func fails
    <A> Either<Error, A> try_ (Recognizer<A> func) {
        Cons cons = input;
        return func.call()
            .fold(e -> { input = cons; return left(e); }
                , a -> right(a));
    }

    Either<Error, Char> char_ () {
        if (input == null) {
            return left(new Error("EOF"));
        }
        int c = input.head();
        if (c == Cons.EOF) {
            return left(new Error("EOF"));
        }
        return right(new Char(input.file, input.line, input.column, c));
    }

    Either<Error, Void> eof () {
        return input == null ? right(null) : left(new Error("expecting end of file"));
    }

    // -------------------- production

    static IntBuffer copy (IntBuffer src) {
        int n = src.position();
        IntBuffer dst = IntBuffer.allocate(n);
        for (int i = 0; i < n; ++i) {
            dst.put(i, src.get(i));
        }
        dst.position(n);
        return dst;
    }

    static String copy_as_string (IntBuffer buffer) {
        StringBuilder s = new StringBuilder();
        for (int i = 0; i < buffer.position(); ++i) {
            s.appendCodePoint(buffer.get(i));
        }
        return s.toString();
    }

    static class Exp {
        Token left;
        Token op;
        Token right;
    }

    static class Op {
        enum Assoc {
            fx,
            xf,
            fy,
            yf,
            xfx,
            xfy,
            yfx,
            yfy,
        }
        int order;
        Assoc assoc;
    }

}
