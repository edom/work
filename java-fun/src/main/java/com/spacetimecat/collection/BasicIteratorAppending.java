package com.spacetimecat.collection;

final class BasicIteratorAppending<A> implements BasicIterator<A>
{
    private final BasicIterator<? extends A> a;
    private final BasicIterator<? extends A> b;

    BasicIteratorAppending (BasicIterator<? extends A> a, BasicIterator<? extends A> b)
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
