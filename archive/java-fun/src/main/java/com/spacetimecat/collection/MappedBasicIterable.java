package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

final class MappedBasicIterable<A, B> implements BasicIterable<B>
{
    private final BasicIterable<A> bi;
    private final BasicFunction1<? super A, B> f;

    MappedBasicIterable (BasicIterable<A> bi, BasicFunction1<? super A, B> f)
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
