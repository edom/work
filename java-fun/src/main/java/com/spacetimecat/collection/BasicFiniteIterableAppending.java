package com.spacetimecat.collection;

final class BasicFiniteIterableAppending<A> implements BasicFiniteIterable<A>
{
    private final BasicFiniteIterable<A> a;
    private final BasicFiniteIterable<? extends A> b;

    BasicFiniteIterableAppending (BasicFiniteIterable<A> a, BasicFiniteIterable<? extends A> b)
    {
        this.a = a;
        this.b = b;
    }

    @Override
    public BasicFiniteIterator<A> iterator ()
    {
        final BasicFiniteIterator<A> c = a.iterator();
        final BasicFiniteIterator<? extends A> d = b.iterator();
        return new BasicFiniteIteratorAppending<>(c, d);
    }
}
