package com.spacetimecat.collection;

public interface BasicFiniteIterable<A> extends BasicIterable<A>
{
    @Override
    BasicFiniteIterator<A> iterator ();
}
