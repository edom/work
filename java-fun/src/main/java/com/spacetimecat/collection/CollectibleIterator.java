package com.spacetimecat.collection;

public interface CollectibleIterator<A>
{
    /**
     * <p>Dump the rest of this iterator to a list.</p>
     * @return a view of that list
     */
    FiniteIterable<A> collectEager ();
}
