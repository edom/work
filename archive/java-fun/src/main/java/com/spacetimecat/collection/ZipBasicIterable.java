package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction2;

final class ZipBasicIterable<A, B, C> implements BasicIterable<C>
{
    private final BasicIterable<A> ia;
    private final BasicIterable<B> ib;
    private final BasicFunction2<A, B, C> f;

    public ZipBasicIterable (BasicIterable<A> ia, BasicIterable<B> ib, BasicFunction2<A, B, C> f)
    {
        this.ia = ia;
        this.ib = ib;
        this.f = f;
    }

    @Override
    public BasicIterator<C> iterator ()
    {
        return new ZipBasicIterator<>(ia.iterator(), ib.iterator(), f);
    }
}
