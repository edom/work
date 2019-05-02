package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>
 *     Function from A to B transforms {@code Mappable<A>} to {@code Mappable<B>}.
 * </p>
 */
public interface Mappable<A>
{
    /**
     * <p>
     *     A view of what would be if {@code f} were applied to each element.
     * </p>
     * <p>
     *     This does not call f immediately.
     * </p>
     * @param <B> result element type
     * @param f mapping function
     * @return a view
     */
    <B> Mappable<B> map (BasicFunction1<? super A, B> f);
}
