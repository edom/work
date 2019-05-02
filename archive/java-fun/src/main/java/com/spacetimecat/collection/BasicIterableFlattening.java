package com.spacetimecat.collection;

final class BasicIterableFlattening<A> implements BasicIterable<A>
{
    private final BasicIterable<BasicIterable<A>> bi;

    BasicIterableFlattening (BasicIterable<BasicIterable<A>> bi)
    {
        this.bi = bi;
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        return new BasicIteratorFlattening<>(new FreeIterator<>(bi.iterator()).map(a -> a.iterator()));
    }
}
