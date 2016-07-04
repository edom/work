package com.spacetimecat.collection;

final class BasicIterableAppending<A> implements BasicIterable<A>
{
    private final BasicIterable<A> a;
    private final BasicIterable<? extends A> b;

    BasicIterableAppending (BasicIterable<A> a, BasicIterable<? extends A> b)
    {
        this.a = a;
        this.b = b;
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        final BasicIterator<A> c = a.iterator();
        final BasicIterator<? extends A> d = b.iterator();
        return new BasicIteratorAppending<>(c, d);
    }
}
