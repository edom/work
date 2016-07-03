package com.spacetimecat.collection;

final class BasicIteratorAppending<A> implements BasicIterator<A>
{
    private final BasicIterator<A> a;
    private final BasicIterator<A> b;

    BasicIteratorAppending (BasicIterator<A> a, BasicIterator<A> b)
    {
        this.a = a;
        this.b = b;
    }

    @Override
    public A next ()
    {
        final A x = a.next();
        if (x != null) { return x; }
        return b.next();
    }
}
