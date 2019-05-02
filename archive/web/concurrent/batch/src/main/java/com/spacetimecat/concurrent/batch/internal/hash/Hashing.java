package com.spacetimecat.concurrent.batch.internal.hash;

/**
 * <p>
 *     Defines hashing for a type.
 * </p>
 *
 * @param <A>
 * thing type
 */
@FunctionalInterface
public interface Hashing<A>
{
    /**
     * <p>
     *     See {@link Object#hashCode()}.
     * </p>
     *
     * @param a
     * the thing whose hash code is to be computed
     *
     * @return
     * the hash code of the thing
     */
    int hashCodeOf (A a);
}
