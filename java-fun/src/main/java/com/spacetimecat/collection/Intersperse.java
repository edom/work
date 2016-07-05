package com.spacetimecat.collection;

/**
 * <p>
 *     Put something between every adjacent pair of elements.
 * </p>
 * <pre>
 *     [].intersperse(x) = []
 *     [a0].intersperse(x) = [a0]
 *     [a0, a1].intersperse(x) = [a0, x, a1]
 *     [a0, a1, a2].intersperse(x) = [a0, x, a1, x, a2]
 *     [a0, a1, ..., an].intersperse(x) = a0 : [x, a1, ..., x, an]
 * </pre>
 * @param <A> element type
 */
public interface Intersperse<A>
{
    /**
     * <p>Put {@code s} between every adjacent pair of elements.</p>
     * @param s separator
     * @return a view
     */
    Intersperse<A> intersperse (A s);
}
