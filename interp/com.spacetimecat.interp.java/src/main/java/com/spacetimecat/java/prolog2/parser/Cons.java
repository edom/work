package com.spacetimecat.java.prolog2.parser;

import java.io.IOException;
import java.io.Reader;
import java.io.UncheckedIOException;

final class Cons {

    static final int EOF = -1;

    final String file;
    final Reader in;
    final int line;
    final int column;

    public static Cons of (String file, Reader in) {
        return new Cons(file, in);
    }

    private Cons (String file, Reader in) {
        this(file, in, 1, 1);
    }

    private Cons (String file, Reader in, int line, int column) {
        this.file = file;
        this.in = in;
        this.line = line;
        this.column = column;
    }

    private boolean read;
    private int h;
    private Cons t;

    void force () {
        if (read) {
            return;
        }
        try {
            h = in.read();
            if (h == EOF) {
                t = null;
            } else {
                final int line2;
                final int column2;
                switch (h) {
                    case '\n':
                        line2 = line + 1;
                        column2 = 1;
                        break;
                    default:
                        line2 = line;
                        column2 = column + 1;
                        break;
                }
                t = new Cons(file, in, line2, column2);
            }
            read = true;
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }
    }

    int head () {
        force();
        return h;
    }

    Cons tail_or_null () {
        force();
        return t;
    }

}
