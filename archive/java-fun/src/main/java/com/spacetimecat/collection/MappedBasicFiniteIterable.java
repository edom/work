package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

final class MappedBasicFiniteIterable<A, B> implements BasicFiniteIterable<B>
{
    private final BasicFiniteIterable<A> bi;
    private final BasicFunction1<? super A, B> f;

    MappedBasicFiniteIterable (BasicFiniteIterable<A> bi, BasicFunction1<? super A, B> f)
    {
        this.bi = bi;
        this.f = f;
    }

    @Override
    public BasicFiniteIterator<B> iterator ()
    {
        return new MappedBasicFiniteIterator<>(bi.iterator(), f);
    }
}
