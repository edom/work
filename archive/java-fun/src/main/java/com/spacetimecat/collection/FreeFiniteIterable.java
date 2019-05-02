package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicPredicate1;

class FreeFiniteIterable<A> extends FreeIterable<A> implements FiniteIterable<A>
{
    private final BasicFiniteIterable<A> bfi;

    public FreeFiniteIterable (BasicFiniteIterable<A> bfi)
    {
        super(bfi);
        this.bfi = bfi;
    }

    @Override
    public FiniteIterable<A> append (BasicFiniteIterable<? extends A> that)
    {
        return new FreeFiniteIterable<>(new BasicFiniteIterableAppending<>(this, that));
    }

    @Override
    public <B> FiniteIterable<B> flatMapFinite (BasicFunction1<? super A, BasicFiniteIterable<B>> f)
    {
        return null;
    }

    @Override
    public FiniteIterator<A> iterator ()
    {
        return new FreeFiniteIterator<>(bfi.iterator());
    }

    @Override
    public int size ()
    {
        return iterator().size();
    }

    @Override
    public <B> FiniteIterable<B> map (BasicFunction1<? super A, B> f)
    {
        return new FreeFiniteIterable<>(new MappedBasicFiniteIterable<>(bfi, f));
    }

    @Override
    public FiniteIterable<A> filter (BasicPredicate1<? super A> p)
    {
        return new FreeFiniteIterable<>(new FilteredBasicFiniteIterable<>(bfi, p));
    }
}
