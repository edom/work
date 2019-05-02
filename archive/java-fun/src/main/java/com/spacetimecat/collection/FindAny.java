package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

/**
 * <p>Provide {@link #findAny(BasicPredicate1)}.</p>
 * @param <A> element type
 */
public interface FindAny<A>
{
    A findAny (BasicPredicate1<? super A> p);
}
