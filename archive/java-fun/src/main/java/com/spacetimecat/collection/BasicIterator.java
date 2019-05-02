package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction0;

/**
 * <p>Generate values of the same type.</p>
 *
 * <p>If you have a {@link BasicIterator}, you can get an {@link Iterator} for free
 * by passing it to {@link Iterators#from(BasicIterator)}.</p>
 *
 * @see BasicFunction0
 */
public interface BasicIterator<A>
{
    /**
     * @return null if there are no more objects
     */
    A next ();
}
