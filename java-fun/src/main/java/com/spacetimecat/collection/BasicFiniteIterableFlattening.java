package com.spacetimecat.collection;

final class BasicFiniteIterableFlattening<A> implements BasicIterable<A>
{
    private final BasicFiniteIterable<BasicFiniteIterable<A>> bi;

    BasicFiniteIterableFlattening (BasicFiniteIterable<BasicFiniteIterable<A>> bi)
    {
        this.bi = bi;
    }

    @Override
    public BasicFiniteIterator<A> iterator ()
    {
        return new BasicFiniteIteratorFlattening<>(new FreeFiniteIterator<>(bi.iterator()).map(a -> a.iterator()));
    }
}
