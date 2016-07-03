package com.spacetimecat.collection;

import java.util.Collection;

/**
 * <p>Can be {@link #dumpTo(Collection) dumped} to standard {@link java.util.Collection}s.</p>
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
    Dumpable<A> dumpTo (java.util.Collection<A> target);
}
