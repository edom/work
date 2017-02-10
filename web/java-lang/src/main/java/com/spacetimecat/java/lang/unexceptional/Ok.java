package com.spacetimecat.java.lang.unexceptional;

import java.util.function.Function;
import java.util.function.Supplier;

public final class Ok<A> extends Risky<A>
{
    private final A value;

    public Ok (A value)
    {
        if (value == null) { throw new NullPointerException("value"); }
        this.value = value;
    }

    @Override
    public boolean isOk ()
    {
        return true;
    }

    @Override
    public <B> Risky<B> then (Function<A, Risky<B>> function)
    {
        return function.apply(value);
    }

    @Override
    public <B> B fold (Function<Throwable, B> ifFail, Function<A, B> ifOk)
    {
        return ifOk.apply(value);
    }

    @Override
    public A getValueOr (A fallback)
    {
        return value;
    }

    @Override
    public A getValueOrGetFrom (Supplier<A> supplier)
    {
        return value;
    }

    @Override
    public <B> Risky<B> mapValue (Function<A, B> function)
    {
        return new Ok<>(function.apply(value));
    }

    @Override
    public Risky<A> mapThrowable (Function<Throwable, Throwable> function)
    {
        return this;
    }

    @Override
    public Risky<A> ifFailRun (Runnable action)
    {
        return this;
    }

    @Override
    public A take () throws Throwable
    {
        return value;
    }
}
