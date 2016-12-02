package com.spacetimecat.build.math.bound;

import java.util.Comparator;

public final class LowerBound<A> extends Bound<A>
{
    public LowerBound (Type type, A value)
    {
        super(type, value);
    }

    public static <A> Comparator<LowerBound<A>> Comparator (Comparator<A> underlying)
    {
        return (a, b) ->
            ValueComparator(underlying)
            .thenComparing(TypeComparator(Type.LowerComparator()))
            .compare(a, b);
    }
}
