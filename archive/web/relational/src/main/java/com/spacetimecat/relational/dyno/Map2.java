package com.spacetimecat.relational.dyno;

/**
 * <p>
 *     Read-only key-value store.
 * </p>
 */
@FunctionalInterface
public interface Map2
{
    /**
     * <p>
     *     Get the value associated with the key.
     * </p>
     * @param key the key
     * @return the value associated with the key
     * @throws RuntimeException if the key is not in this map
     */
    Object get (String key);
}
