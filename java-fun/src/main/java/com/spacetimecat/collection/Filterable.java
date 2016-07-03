package com.spacetimecat.collection;

import com.spacetimecat.function.BasicPredicate1;

/**
 * <p>Can be {@link #filter(BasicPredicate1) filter}ed.</p>
 * @see BasicPredicate1
 * @see Mappable
 * @param <A> element type
 */
public interface Filterable<A>
{
    /**
     * <p>The elements that satisfy the predicate
     * (the elements for which the predicate returns true).</p>
     * @param p the predicate
     * @return a view of p-satisfying elements
     */
    Filterable<A> filter (BasicPredicate1<A> p);
}
