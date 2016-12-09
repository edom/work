package com.spacetimecat.java.lang.unexceptional;

import java.util.concurrent.Callable;
import java.util.function.Supplier;

public final class RiskyCallable<A> implements Supplier<Risky<A>>
{
    private final Callable<A> delegate;

    public RiskyCallable (Callable<A> delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Risky<A> get ()
    {
        try
        {
            final A value = delegate.call();
            return new Right<>(value);
        }
        catch (Throwable e)
        {
            return new Left<>(e);
        }
    }
}
