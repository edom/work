package com.spacetimecat.collection;

import java.lang.*;
import java.lang.Iterable;

public final class StandardBasicIterable<A> implements BasicIterable<A>
{
    private final java.lang.Iterable<A> i;

    public StandardBasicIterable (Iterable<A> i)
    {
        this.i = i;
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        return new StandardBasicIterator<>(i.iterator());
    }
}
