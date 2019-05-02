package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

final class FilteredBasicIterator<A> implements BasicIterator<A>
{
    private final BasicIterator<A> bi;
    private final BasicPredicate1<? super A> p;

    FilteredBasicIterator (BasicIterator<A> bi, BasicPredicate1<? super A> p)
    {
        this.bi = bi;
        this.p = p;
    }

    @Override
    public A next ()
    {
        A a;
        while ((a = bi.next()) != null)
        {
            if (p.at(a)) { return a; }
        }
        return null;
    }
}
