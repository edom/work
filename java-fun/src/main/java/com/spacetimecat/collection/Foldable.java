package com.spacetimecat.collection;

import com.spacetimecat.function.Function2;

public interface Foldable<A>
{
    /**
     * <p>Eager left-fold.</p>
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
    <B> B fold (B e, Function2<B, A, B> f);
}
