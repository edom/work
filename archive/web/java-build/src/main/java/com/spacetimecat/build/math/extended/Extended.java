package com.spacetimecat.build.math.extended;

/**
 * <p>
 *     {@code Extended<A>} is {@code A} extended with negative infinity and positive infinity.
 * </p>
 *
 * @param <A> value type
 */
public abstract class Extended<A>
{
    public boolean isNegativeInfinity ()
    {
        return false;
    }

    public boolean isPositiveInfinity ()
    {
        return false;
    }

    public final boolean isFinite ()
    {
        return !(isNegativeInfinity() || isPositiveInfinity());
    }

    public A value ()
    {
        throw new UnsupportedOperationException();
    }

    @Override
    public String toString ()
    {
        return value().toString();
    }
}
