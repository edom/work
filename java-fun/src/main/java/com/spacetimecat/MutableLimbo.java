package com.spacetimecat;

import com.spacetimecat.collection.BasicMutableCollection;
import com.spacetimecat.collection.HasImmutableFacade;

/**
 * <p>{@link Limbo} in the making.</p>
 *
 * <p>See {@link Limbo} for usage.</p>
 */
public interface MutableLimbo extends
    BasicMutableCollection<AutoCloseable>
    , HasImmutableFacade<AutoCloseable>
    , Limbo
{
    @Override
    MutableLimbo add (AutoCloseable resource);

    @Override
    Limbo immutable ();

    /**
     * <p>This is {@link #add(AutoCloseable) add},
     * but this returns the {@link AutoCloseable} argument instead of the {@link Limbo} instance.</p>
     * @param resource an {@link AutoCloseable} instance
     * @param <B> resource type
     * @return the same {@link AutoCloseable} instance as the argument
     */
    <B extends AutoCloseable> B register (B resource);
}
