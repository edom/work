package com.spacetimecat.build.math.extended;

import java.io.Serializable;
import java.util.Comparator;

public final class ExtendedComparator<A> implements Comparator<Extended<A>>, Serializable
{
    private final Comparator<A> underlying;

    public ExtendedComparator (Comparator<A> underlying)
    {
        this.underlying = underlying;
    }

    @Override
    public int compare (Extended<A> a, Extended<A> b)
    {
        if (a.isNegativeInfinity())
        {
            if (b.isNegativeInfinity()) { return 0; }
            return -1;
        }
        if (a.isPositiveInfinity())
        {
            if (b.isPositiveInfinity()) { return 0; }
            return 1;
        }
        return underlying.compare(a.value(), b.value());
    }
}
