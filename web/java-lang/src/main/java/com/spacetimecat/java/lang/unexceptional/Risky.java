package com.spacetimecat.java.lang.unexceptional;

import java.util.function.Consumer;
import java.util.function.Function;
import java.util.function.Supplier;

/**
 * <p>
 *     Something that produces either a {@link Throwable} or an {@code A}.
 * </p>
 *
 * <p>
 *     A method returns this to signal that it may fail,
 *     but it does not want to throw anything to indicate that failure.
 * </p>
 *
 * <p>
 *     This is similar to checked exceptions
 *     in the sense that this makes the compiler
 *     force the caller to do something.
 * </p>
 *
 * @param <A> wrapped type in the successful case
 */
public abstract class Risky<A>
{
    // No subclass outside this package.
    Risky () {}

    public boolean isOk ()
    {
        return false;
    }

    public A getValueOr (A fallback)
    {
        return fallback;
    }

    public A getValueOrAssert ()
    {
        throw new AssertionError();
    }

    public Risky<A> ifOkDo (Runnable action)
    {
        return this;
    }

    public Risky<A> ifFailRun (Runnable action)
    {
        action.run();
        return this;
    }

    public Risky<A> ifFailDo (Consumer<Throwable> consumer)
    {
        return this;
    }

    public A getValueOrGetFrom (Supplier<A> supplier)
    {
        return supplier.get();
    }

    public Throwable getThrowableOr (Throwable fallback)
    {
        return fallback;
    }

    public abstract <B> Risky<B> then (Function<A, Risky<B>> function);

    public void throwChecked () throws Throwable
    {
    }

    public void throwUnchecked ()
    {
    }

    public abstract <B> Risky<B> mapValue (Function<A, B> function);

    public abstract Risky<A> mapThrowable (Function<Throwable, Throwable> function);

    public abstract <B> B fold (Function<Throwable, B> ifFail, Function<A, B> ifOk);
}
