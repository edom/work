package com.spacetimecat.collection;

/**
 * <p>Can be {@link #dumpTo(java.util.Collection) dumped} to standard {@link java.util.Collection}s.</p>
 *
 * @param <A> element type
 */
public interface Dumpable<A>
{
    /**
     * <p>Copy everything to the given collection.</p>
     *
     * @param target the collection receiving the dump
     *
     * @return this
     */
    Dumpable<A> dumpTo (java.util.Collection<? super A> target);
}
