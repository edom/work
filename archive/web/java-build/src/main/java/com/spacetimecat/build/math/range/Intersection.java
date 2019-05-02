package com.spacetimecat.build.math.range;

import com.spacetimecat.build.math.bound.LowerBound;
import com.spacetimecat.build.math.bound.UpperBound;

import java.util.Comparator;

public final class Intersection<A>
{
    private final Comparator<A> underlying;
    private final Range<A> a;
    private final Range<A> b;

    public Intersection (Comparator<A> underlying, Range<A> a, Range<A> b)
    {
        this.underlying = underlying;
        this.a = a;
        this.b = b;
    }

    public Range<A> compute ()
    {
        final LowerBound<A> lowerBound = new Comparator2<>(LowerBound.Comparator(underlying)).max(a.lowerBound(), b.lowerBound());
        final UpperBound<A> upperBound = new Comparator2<>(UpperBound.Comparator(underlying)).min(a.upperBound(), b.upperBound());
        return new Range<>(lowerBound, upperBound);
    }

    private static class Comparator2<A>
    {
        private final Comparator<A> comparator;

        private Comparator2 (Comparator<A> comparator)
        {
            this.comparator = comparator;
        }

        public A min (A a, A b)
        {
            return comparator.compare(a, b) <= 0 ? a : b;
        }

        public A max (A a, A b)
        {
            return comparator.compare(a, b) <= 0 ? b : a;
        }
    }
}
