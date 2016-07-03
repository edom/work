package com.spacetimecat.collection;

import java.util.Arrays;

public final class Iterables
{
    private Iterables () {}

    public static <A> Iterable<A> from (java.lang.Iterable<A> i)
    {
        return new FreeIterable(new BasicIterableFromJavaLang(i));
    }

    public static <A> Iterable<A> from (A... as)
    {
        return from(Arrays.asList(as));
    }

    /**
     * <p>This creates a representation of a sequence of integers going up.</p>
     *
     * @param incBegin value to begin with, included in the sequence
     * @param excEnd value to end with, but not included in the sequence
     * @param increment the difference between two adjacent integers
     *
     * @return an {@link Iterable} representation;
     * the integers are not generated immediately
     */
    public static Iterable<Integer> risingIntegerSequence (int incBegin, int excEnd, int increment)
    {
        return new FreeIterable<>(new RisingIntegerSequence(incBegin, excEnd, increment));
    }

}
