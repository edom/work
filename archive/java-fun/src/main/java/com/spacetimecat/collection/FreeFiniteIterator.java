package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

class FreeFiniteIterator<A> extends FreeIterator<A> implements FiniteIterator<A>
{
    private final BasicFiniteIterator<A> bfi;
    public FreeFiniteIterator (BasicFiniteIterator<A> bfi)
    {
        super(bfi);
        this.bfi = bfi;
    }

    @Override
    public int size ()
    {
        return fold(0, (n, e) -> n + 1);
    }

    @Override
    public <B> FiniteIterator<B> map (BasicFunction1<? super A, B> f)
    {
        return new FreeFiniteIterator<>(new MappedBasicFiniteIterator<>(bfi, f));
    }
}
