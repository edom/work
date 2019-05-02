package com.spacetimecat.collection;

public interface BasicMutableMap<K, V>
{
    /**
     * <p>Associate the key to the value.</p>
     * @param k key
     * @param v value
     * @return this
     */
    BasicMutableMap<K, V> put (K k, V v);
}
