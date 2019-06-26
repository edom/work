package com.spacetimecat.java.prolog;

public class Prolog_Exception extends RuntimeException {

    private static final long serialVersionUID = 1L;

    public Prolog_Exception () {
        super();
    }

    public Prolog_Exception (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public Prolog_Exception (String message, Throwable cause) {
        super(message, cause);
    }

    public Prolog_Exception (String message) {
        super(message);
    }

    public Prolog_Exception (Throwable cause) {
        super(cause);
    }

}
