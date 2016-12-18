package com.spacetimecat.java.lang.group;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

/**
 * <p>
 *     Computes equivalence classes in {@code O(n * log n)} time in the worst case,
 *     given a comparator, where {@code n} is the number of things in the input list.
 * </p>
 *
 * @param <A>
 * element type
 */
public final class ByComparator<A> extends GroupThings<A>
{
    private final Comparator<A> comparator;

    public ByComparator (Comparator<A> comparator)
    {
        this.comparator = comparator;
    }

    @Override
    public List<List<A>> group (List<A> theThings)
    {
        final List<A> things = new ArrayList<>(theThings);
        final int size = things.size();

        final List<List<A>> classes = new ArrayList<>(size);

        things.sort(comparator);

        int begin = 0;
        while (begin < size)
        {
            final List<A> cls = new ArrayList<>(size);

            final A representative = things.get(begin);

            cls.add(representative);

            int end;
            for (end = begin + 1; end < size; ++end)
            {
                final A thing = things.get(end);
                if (!isEquivalent(representative, thing)) { break; }
                cls.add(thing);
            }
            begin = end;

            classes.add(cls);
        }

        return classes;
    }

    private boolean isEquivalent (A x, A y)
    {
        return comparator.compare(x, y) == 0;
    }
}
