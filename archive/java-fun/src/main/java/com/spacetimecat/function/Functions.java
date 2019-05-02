package com.spacetimecat.function;

public final class Functions
{
    private Functions () {}

    public static <A, B> Function1<A, B> from (BasicFunction1<A, B> f)
    {
        return new FreeFunction1<>(f);
    }

    public static <A, B, C> BasicFunction1<A, C> compose (BasicFunction1<B, C> f, BasicFunction1<A, B> g)
    {
        return new ComposedBasicFunction1<>(f, g);
    }
}
