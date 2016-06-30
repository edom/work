package com.spacetimecat.collection;

/**
 * <p>This generates values.</p>
 */
public interface BasicIterator<A>
{
    /**
     * @return null if there are no more objects
     */
    A next ();
}
