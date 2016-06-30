package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

/**
 * <p>If you have a {@link BasicIterator}, you can get this for free by using {@link FreeIterator}.</p>
 */
public interface Iterator<A> extends BasicIterator<A>, Foldable<A>
{
    /**
     * <p>This calls {@link #next()} n times</p>
     * @return this
     */
    Iterator<A> skip (int n);

    /**
     * @return this, although it might be useless because this method exhausts this iterator
     */
    Iterator<A> forEach (Procedure1<A> f);

    <B> Iterator<B> map (Function1<A, B> f);

    <B, C> Iterator<C> zip (BasicIterator<B> that, Function2<A, B, C> f);
}
