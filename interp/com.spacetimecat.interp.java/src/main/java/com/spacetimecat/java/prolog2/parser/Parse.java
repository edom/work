package com.spacetimecat.java.prolog2.parser;

abstract class Parse {

    Parse error (String message) {
        return new Error(message);
    }

    final class Error extends Parse {

        final String message;

        public Error (String message) {
            this.message = message;
        }

    }

    class Tree extends Parse {
    }

}
