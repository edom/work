package com.spacetimecat.concurrent.batch.internal.hash;

/**
 * <p>
 *     Defines equivalence for a type.
 * </p>
 *
 * @param <A>
 * thing type
 */
@FunctionalInterface
public interface Equivalence<A>
{
    /**
     * <p>
     *     This predicate must represent a
     *     reflexive, symmetric, and transitive relation.
     * </p>
     *
     * <p>
     *     Reflexive: {@code areEquivalent(x, x)} is true.
     * </p>
     *
     * <p>
     *     Symmmetric: {@code areEquivalent(x, y) == areEquivalent(y, x)}.
     * </p>
     *
     * <p>
     *     Transitive: If {@code areEquivalent(x, y)}
     *     and {@code areEquivalent(y, z)},
     *     then {@code areEquivalent(x, z)}.
     * </p>
     *
     * @param a
     * the first thing
     *
     * @param b
     * the second thing
     *
     * @return
     * whether this predicate considers them equivalent
     */
    boolean areEquivalent (A a, A b);
}
