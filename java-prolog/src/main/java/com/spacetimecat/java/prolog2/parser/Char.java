package com.spacetimecat.java.prolog2.parser;

class Char {

    final String file;
    final int line;
    final int column;
    final int code;

    public Char (String file, int line, int column, int code) {
        this.file = file;
        this.line = line;
        this.column = column;
        this.code = code;
    }

}