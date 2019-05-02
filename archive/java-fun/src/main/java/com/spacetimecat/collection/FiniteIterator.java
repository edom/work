package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

public interface FiniteIterator<A> extends
    BasicFiniteIterator<A>
    , Iterator<A>
    , Sized
{
    @Override
    <B> FiniteIterator<B> map (BasicFunction1<? super A, B> f);
}
