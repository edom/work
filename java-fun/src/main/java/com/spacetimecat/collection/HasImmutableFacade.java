package com.spacetimecat.collection;

/**
 * <p>Can wrap itself into an {@link #immutable() immutable} {@link BasicCollection}.</p>
 * @param <A> element type
 */
public interface HasImmutableFacade<A>
{
    /**
     * <p>Wrap this in an immutable facade.</p>
     * <p>This should satisfy {@code !(immutable() instanceof BasicMutableCollection)}
     * to prevent easy circumvention.</p>
     * @return not a {@link BasicMutableCollection}
     */
    BasicCollection<A> immutable ();
}
