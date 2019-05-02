package com.spacetimecat.concurrent.batch.internal.hash;

import java.util.Objects;

/**
 * <p>
 *     Uses {@link Objects#hashCode(Object) hashCode}
 *     that comes with the standard library.
 * </p>
 *
 * @param <A>
 * thing type
 */
public final class DefaultHashing<A> implements Hashing<A>
{
    @Override
    public int hashCodeOf (A a)
    {
        return Objects.hashCode(a);
    }
}
