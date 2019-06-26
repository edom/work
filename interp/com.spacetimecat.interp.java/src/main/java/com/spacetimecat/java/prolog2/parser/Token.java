package com.spacetimecat.java.prolog2.parser;

class Token {
    String file;
    int line;
    int column;
    String text;

    @Override
    public String toString () {
        return String.format("%s:%d:%d:%s", file, line, column, text);
    }
}