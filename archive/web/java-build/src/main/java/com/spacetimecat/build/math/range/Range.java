package com.spacetimecat.build.math.range;

import com.spacetimecat.build.math.bound.LowerBound;
import com.spacetimecat.build.math.bound.UpperBound;

/**
 * @param <A> type of a point
 */
public final class Range<A>
{
    private final LowerBound<A> lower;
    private final UpperBound<A> upper;

    public Range (LowerBound<A> lower, UpperBound<A> upper)
    {
        this.lower = lower;
        this.upper = upper;
    }

    @Override
    public String toString ()
    {
        return String.format("%s - %s", lower, upper);
    }

    public LowerBound<A> lowerBound ()
    {
        return lower;
    }

    public UpperBound<A> upperBound ()
    {
        return upper;
    }
}
