package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

final class FilteredBasicFiniteIterator<A> implements BasicFiniteIterator<A>
{
    private final BasicFiniteIterator<A> bi;
    private final BasicPredicate1<? super A> p;

    FilteredBasicFiniteIterator (BasicFiniteIterator<A> bi, BasicPredicate1<? super A> p)
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
