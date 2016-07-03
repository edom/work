package com.spacetimecat;

/**
 * <p>Replace {@link Void} where nulls cannot be used.</p>
 */
public final class Null
{
    private static final Null instance = new Null();

    private Null () {}

    public static Null get ()
    {
        return instance;
    }
}
