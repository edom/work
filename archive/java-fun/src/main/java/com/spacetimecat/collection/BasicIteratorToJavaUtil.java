package com.spacetimecat.collection;

import java.util.NoSuchElementException;

final class BasicIteratorToJavaUtil<A> implements java.util.Iterator<A>
{
    private final BasicIterator<A> bi;
    private A a;

    BasicIteratorToJavaUtil (BasicIterator<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public boolean hasNext ()
    {
        if (a == null) { a = bi.next(); }
        return a != null;
    }

    @Override
    public A next ()
    {
        if (!hasNext())
        {
            throw new NoSuchElementException();
        }
        assert a != null;
        return a;
    }
}
