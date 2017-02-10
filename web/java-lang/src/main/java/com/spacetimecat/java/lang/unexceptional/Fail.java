package com.spacetimecat.java.lang.unexceptional;

import com.spacetimecat.java.lang.Unchecked;

import java.util.function.Consumer;
import java.util.function.Function;

public final class Fail<A> extends Risky<A>
{
    private final Throwable throwable;

    public Fail (Throwable throwable)
    {
        if (throwable == null) { throw new NullPointerException("throwable"); }
        this.throwable = throwable;
    }

    @Override
    public Throwable getThrowableOr (Throwable fallback)
    {
        return throwable;
    }

    @Override
    public Risky<A> ifFailDo (Consumer<Throwable> consumer)
    {
        consumer.accept(throwable);
        return this;
    }

    @Override
    public <B> Risky<B> then (Function<A, Risky<B>> function)
    {
        return new Fail<>(throwable);
    }

    @Override
    public void throwChecked () throws Throwable
    {
        throw throwable;
    }

    @Override
    public void throwUnchecked ()
    {
        new Unchecked(throwable).raise();
    }

    @Override
    public <B> Risky<B> mapValue (Function<A, B> function)
    {
        return new Fail<>(throwable);
    }

    @Override
    public Risky<A> mapThrowable (Function<Throwable, Throwable> function)
    {
        return new Fail<>(function.apply(throwable));
    }

    @Override
    public <B> B fold (Function<Throwable, B> ifFail, Function<A, B> ifOk)
    {
        return ifFail.apply(throwable);
    }
}
