package com.spacetimecat.java.lang.unexceptional;

import java.util.function.Function;

public final class RiskyFunction<A, B> implements Function<A, Risky<B>>
{
    private final Function<A, B> original;

    public RiskyFunction (Function<A, B> original)
    {
        this.original = original;
    }

    @Override
    public Risky<B> apply (A a)
    {
        try
        {
            final B value = original.apply(a);
            return new Right<>(value);
        }
        catch (Throwable e)
        {
            return new Left<>(e);
        }
    }
}
