package com.spacetimecat.collection;

final class BasicFiniteIteratorAppending<A> implements BasicFiniteIterator<A>
{
    private final BasicFiniteIterator<? extends A> a;
    private final BasicFiniteIterator<? extends A> b;

    BasicFiniteIteratorAppending (BasicFiniteIterator<? extends A> a, BasicFiniteIterator<? extends A> b)
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
