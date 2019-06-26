package com.spacetimecat.interp.java.lispy;

import java.util.Objects;

public final class Error_Value {

    private final Object what;

    private Error_Value (Object message) {
        this.what = message;
    }

    public static Error_Value from (Object what) {
        return new Error_Value(what);
    }

    public static Error_Value from_new_RuntimeException (String message) {
        return new Error_Value(new RuntimeException(message));
    }

    @Override
    public String toString () {
        return Objects.toString(what);
    }

}
