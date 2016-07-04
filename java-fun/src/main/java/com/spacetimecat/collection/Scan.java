package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction2;

/**
 * <p>If you have a {@link Scan}, you can get a {@link Foldable} for free.</p>
 *
 * @see Foldable
 *
 * @param <A> element type
 */
public interface Scan<A>
{
    /**
     * A sequence of partial folds.
     *
     * @param e initial result
     * @param f reducing function
     * @param <B> result type
     * @return a sequence of partial left-folds b0, b1, ...
     * where b0 = e and b[n] = f b[N-1] a[n],
     * where a0, a1, ... is the sequence of elements
     * produced by the original iterator.
     */
    <B> BasicIterator<B> scan (B e, BasicFunction2<B, ? super A, B> f);
}
