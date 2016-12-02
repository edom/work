package com.spacetimecat.build.math.bound;

import java.util.Comparator;

public abstract class Bound<A>
{
    private final Type type;
    private final A value;

    protected Bound (Type type, A value)
    {
        this.type = type;
        this.value = value;
    }

    public final Type type () { return type; }

    public final A value ()
    {
        return value;
    }

    @Override
    public final String toString ()
    {
        return String.format("%s %s", type, value);
    }

    protected static <A> Comparator<Bound<A>> ValueComparator (Comparator<A> underlying)
    {
        return (a, b) -> underlying.compare(a.value(), b.value());
    }

    protected static <A> Comparator<Bound<A>> TypeComparator (Comparator<Type> underlying)
    {
        return (a, b) -> underlying.compare(a.type, b.type);
    }
}
