package com.spacetimecat.collection;

/**
 * <p>Extend {@link BasicMap} with {@link #getOr(Object, Object) getOr}.</p>
 * @param <K> key type
 * @param <V> value type
 */
public interface DefaultingMap<K, V> extends BasicMap<K, V>
{
    V getOr (K k, V def);
}
