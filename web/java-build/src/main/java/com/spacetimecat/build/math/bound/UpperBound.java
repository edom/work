package com.spacetimecat.build.math.bound;

import java.util.Comparator;

public final class UpperBound<A> extends Bound<A>
{
    public UpperBound (Type type, A value)
    {
        super(type, value);
    }

    public static <A> Comparator<UpperBound<A>> Comparator (Comparator<A> underlying)
    {
        return (a, b) ->
            ValueComparator(underlying)
            .thenComparing(TypeComparator(Type.UpperComparator()))
            .compare(a, b);
    }
}
