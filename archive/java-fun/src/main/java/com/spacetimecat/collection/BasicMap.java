package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>Find the value associated with a given key.</p>
 *
 * @see Indexed
 * @see BasicFunction1
 */
public interface BasicMap<K, V>
{
    /**
     * <p>Look up the key in the map.</p>
     *
     * @param k the key, cannot be null
     *
     * @return the value associated with the key,
     * or null if the key is not in the map
     */
    V get (K k);
}
