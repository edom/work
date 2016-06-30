package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;

final class MappedBasicIterable<A, B> implements BasicIterable<B>
{
    private final BasicIterable<A> bi;
    private final Function1<A, B> f;

    MappedBasicIterable (BasicIterable<A> bi, Function1<A, B> f)
    {
        this.bi = bi;
        this.f = f;
    }

    @Override
    public BasicIterator<B> iterator ()
    {
        return new MappedBasicIterator<>(bi.iterator(), f);
    }
}
