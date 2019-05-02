package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction2;

/**
 * <p>Eager left {@link #fold(Object, BasicFunction2) fold}.</p>
 *
 * <p>A collection, iterable, iterator, stream,
 * or whatever that has a left fold.</p>
 *
 * @param <A> element type
 */
public interface Foldable<A>
{
    /**
     * <p>Eager left fold.</p>
     *
     * <p>Right-fold can be done by swapping the arguments of the reducing function.</p>
     *
     * @param e initial result
     * @param f reducing function
     *
     * @param <B> result type
     *
     * @return f ... f (f (f e a0) a1) a2 ... a[n-1]
     */
    <B> B fold (B e, BasicFunction2<B, A, B> f);
}
