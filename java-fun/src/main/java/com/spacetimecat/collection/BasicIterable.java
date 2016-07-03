package com.spacetimecat.collection;

/**
 * <p>Make {@link BasicIterator}s.</p>
 * @param <A> element type
 */
public interface BasicIterable<A>
{
    BasicIterator<A> iterator ();
}
