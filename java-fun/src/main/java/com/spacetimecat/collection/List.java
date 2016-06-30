package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

/**
 * <p>This is an {@link Iterable} with indexed access.</p>
 */
public interface List<A> extends Iterable<A>
{
    /**
     * @return null if the index is out of bounds
     */
    A at (int i);

    @Override
    List<A> forEach (Procedure1<A> f);

    @Override
    <B> List<B> map (Function1<A, B> f);

    @Override
    <B, C> List<C> zip (BasicIterable<B> bs, Function2<A, B, C> f);

    /**
     * <p>You can do whatever you want to the returned list.
     * It will not affect this list.</p>
     *
     * <p>If the implementation has a backing list,
     * it returns a copy of that backing list.</p>
     */
    java.util.List<A> copyStdList ();
}
