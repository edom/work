package com.spacetimecat.concurrent.batch.internal.hash;

import java.util.Objects;

/**
 * <p>
 *     Uses {@link Objects#equals(Object, Object) equals}
 *     that comes with the standard library.
 * </p>
 *
 * @param <A>
 * thing type
 */
public final class DefaultEquivalence<A> implements Equivalence<A>
{
    @Override
    public boolean areEquivalent (A a, A b)
    {
        return Objects.equals(a, b);
    }
}
