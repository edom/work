package com.spacetimecat.collection;

import com.spacetimecat.function.Function2;

public final class ZipBasicIterable<A, B, C> implements BasicIterable<C>
{
    private final BasicIterable<A> ia;
    private final BasicIterable<B> ib;
    private final Function2<A, B, C> f;

    public ZipBasicIterable (BasicIterable<A> ia, BasicIterable<B> ib, Function2<A, B, C> f)
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
