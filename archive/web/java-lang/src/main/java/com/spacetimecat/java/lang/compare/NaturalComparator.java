package com.spacetimecat.java.lang.compare;

import java.io.Serializable;
import java.util.Comparator;

/**
 * <p>
 *     {@link Comparator} that uses the underlying class's natural ordering.
 * </p>
 * @param <A> a type having a natural order
 */
public final class NaturalComparator<A extends Comparable<A>> implements Comparator<A>, Serializable
{
    @Override
    public int compare (A a, A b)
    {
        return a.compareTo(b);
    }
}
