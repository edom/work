package com.spacetimecat.build.math.vector;

import java.util.Comparator;
import java.util.Objects;

/**
 * <p>
 *     Lexicographical-like ordering of vectors.
 * </p>
 * @param <P> vector element type
 */
public final class PrefixComparator<P> implements Comparator<Vector<P>>
{
    private final Comparator<P> partwise;

    public PrefixComparator (Comparator<P> partwise)
    {
        Objects.requireNonNull(partwise);
        this.partwise = partwise;
    }

    @Override
    public int compare (Vector<P> a, Vector<P> b)
    {
        final int size = Math.min(a.size(), b.size());
        for (int i = 0; i < size; ++i)
        {
            final int part = partwise.compare(a.get(i), b.get(i));
            if (part != 0) { return part; }
        }
        return a.size() - b.size();
    }
}
