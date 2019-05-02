package com.spacetimecat.java.lang.unexceptional;

import com.spacetimecat.java.lang.Unchecked;

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

    public abstract <B> Risky<B> mapValue (Function<A, B> function);

    public abstract Risky<A> mapThrowable (Function<Throwable, Throwable> function);

    public abstract <B> B fold (Function<Throwable, B> ifFail, Function<A, B> ifOk);

    /**
     * <p>
     *     Get the value if this is an {@link Ok},
     *     or throw an exception if this is a {@link Fail}.
     * </p>
     *
     * <p>
     *     Convert monad-based code into exception-based code.
     * </p>
     *
     * @return
     * the value if this is an {@link Ok}
     *
     * @throws Throwable
     * if this is a {@link Fail}
     */
    public abstract A take () throws Throwable;

    /**
     * <p>
     *     This is like {@link #take()} but throws only unchecked throwables.
     * </p>
     *
     * <p>
     *     If the throwable is unchecked, this throws it as is.
     *     If the throwable is checked, this wraps it in a {@link com.spacetimecat.java.lang.UncheckedException}.
     * </p>
     *
     * @return
     * the value if this is an {@link Ok}
     */
    public final A takeUnchecked ()
    {
        try
        {
            return take();
        }
        catch (Throwable e)
        {
            new Unchecked(e).raise();
            throw new AssertionError("should not reach here");
        }
    }
}
