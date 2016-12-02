package com.spacetimecat.java.lang.compare;

import java.util.Comparator;

/**
 * <p>
 *     {@link Comparable} from a {@link Comparator}.
 * </p>
 * @param <A> underlying type
 */
public final class ArtificialComparable<A> implements Comparable<ArtificialComparable<A>>
{
    private final Comparator<A> comparator;
    private final A value;

    public ArtificialComparable (Comparator<A> comparator, A value)
    {
        this.comparator = comparator;
        this.value = value;
    }

    @Override
    public int compareTo (ArtificialComparable<A> that)
    {
        return comparator.compare(this.value, that.value);
    }

    @Override
    public String toString ()
    {
        return String.format("%s %s", comparator, value);
    }
}
