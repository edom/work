package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

final class FilteredBasicFiniteIterable<A> implements BasicFiniteIterable<A>
{
    private final BasicFiniteIterable<A> bi;
    private final BasicPredicate1<? super A> p;

    FilteredBasicFiniteIterable (BasicFiniteIterable<A> bi, BasicPredicate1<? super A> p)
    {
        this.bi = bi;
        this.p = p;
    }

    @Override
    public BasicFiniteIterator<A> iterator ()
    {
        return new FilteredBasicFiniteIterator<>(bi.iterator(), p);
    }
}
