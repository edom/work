package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

final class FilteredBasicIterable<A> implements BasicIterable<A>
{
    private final BasicIterable<A> bi;
    private final BasicPredicate1<? super A> p;

    FilteredBasicIterable (BasicIterable<A> bi, BasicPredicate1<? super A> p)
    {
        this.bi = bi;
        this.p = p;
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        return new FilteredBasicIterator<>(bi.iterator(), p);
    }
}
