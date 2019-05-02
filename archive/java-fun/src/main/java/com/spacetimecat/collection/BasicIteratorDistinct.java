package com.spacetimecat.collection;

import java.util.HashSet;
import java.util.Set;

final class BasicIteratorDistinct<A> implements BasicIterator<A>
{
    private final BasicIterator<A> bi;
    private Set<A> set = new HashSet<>();

    BasicIteratorDistinct (BasicIterator<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public A next ()
    {
        A a;
        do { a = bi.next(); }
        while (a != null && !set.add(a));
        return a;
    }
}
