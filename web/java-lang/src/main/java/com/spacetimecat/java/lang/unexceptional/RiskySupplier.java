package com.spacetimecat.java.lang.unexceptional;

import java.util.function.Supplier;

/**
 * <p>
 *     Convert exception-based code to conditional-based code.
 * </p>
 *
 * <p>
 *     If the code you want to wrap throws checked exceptions,
 *     use {@link RiskyCallable}.
 * </p>
 *
 * @param <A>
 * wrapped value type
 */
public final class RiskySupplier<A> implements Supplier<Risky<A>>
{
    private final Supplier<A> delegate;

    /**
     * <p>
     *     Convert exception-based code to conditional-based code.
     * </p>
     *
     * <p>
     *     The returned supplier will not throw anything.
     * </p>
     *
     * @param delegate
     * the original supplier that may throw something
     */
    public RiskySupplier (Supplier<A> delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Risky<A> get ()
    {
        try
        {
            final A value = delegate.get();
            return new Right<>(value);
        }
        catch (Throwable e)
        {
            return new Left<>(e);
        }
    }
}
