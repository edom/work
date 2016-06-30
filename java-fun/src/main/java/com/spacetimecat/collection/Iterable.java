package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

public interface Iterable<A> extends BasicIterable<A>, Foldable<A>
{
    /**
     * @return this
     */
    Iterable<A> forEach (Procedure1<A> f);

    <B> Iterable<B> map (Function1<A, B> f);

    <B, C> Iterable<C> zip (BasicIterable<B> bs, Function2<A, B, C> f);
}
