package com.spacetimecat.java.lang.unexceptional;

import java.util.function.Function;

public final class RiskyFunction<A, B> implements Function<A, Risky<B>>
{
    private final FunctionE<A, B> original;

    public RiskyFunction (FunctionE<A, B> original)
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
