package com.spacetimecat.collection;

/**
 * <p>Extend {@link BasicMap} with {@link #keys() keys} and {@link #values() values}.</p>
 * @param <K> key type
 * @param <V> value type
 */
public interface BasicMap0<K, V> extends BasicMap<K, V>
{
    Iterable<K> keys ();

    Iterable<V> values ();
}
