package com.spacetimecat.collection;

import java.util.Arrays;

public final class Iterables
{
    private Iterables () {}

    public static <A> FiniteIterable<A> empty ()
    {
        return from();
    }

    public static <A> Iterable<A> fromInfinite (java.lang.Iterable<A> i)
    {
        return new FreeIterable(new BasicIterableFromJavaLang(i));
    }

    public static <A> FiniteIterable<A> from (java.lang.Iterable<A> i)
    {
        return new FreeFiniteIterable<>(new BasicIterableFromJavaLang<>(i));
    }

    public static <A> FiniteIterable<A> from (A... as)
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
    public static FiniteIterable<Integer> risingIntegerSequence (int incBegin, int excEnd, int increment)
    {
        return new FreeFiniteIterable<>(new RisingIntegerSequence(incBegin, excEnd, increment));
    }

}
