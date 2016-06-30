package com.spacetimecat.collection;

public interface MutableList<A> extends List<A>
{
    /**
     * @return this
     */
    MutableList<A> add (A a);
}
