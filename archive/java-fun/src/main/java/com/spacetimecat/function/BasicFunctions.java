package com.spacetimecat.function;

import java.util.concurrent.Callable;

public final class BasicFunctions
{
    private BasicFunctions () {}

    public static <A> BasicFunction0<A> constant (A c)
    {
        return () -> c;
    }

    public static <A> BasicFunction0<A> from (Callable<A> c)
    {
        return new BasicFunction0FromCallable<>(c);
    }
}
