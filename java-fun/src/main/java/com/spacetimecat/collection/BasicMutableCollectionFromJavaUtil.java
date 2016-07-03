package com.spacetimecat.collection;

import java.util.Collection;

final class BasicMutableCollectionFromJavaUtil<A> implements BasicMutableCollection<A>
{
    private final java.util.Collection<A> c;

    BasicMutableCollectionFromJavaUtil (Collection<A> c)
    {
        this.c = c;
    }

    @Override
    public BasicMutableCollection<A> add (A a)
    {
        c.add(a);
        return this;
    }
}
