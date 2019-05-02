package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>Associate each key to a value.</p>
 *
 * <p>A {@link Map} is also a {@link BasicFunction1} due to {@link Indexed}.</p>
 *
 * @param <K> key type
 * @param <V> value type
 */
public interface Map<K, V> extends
    BasicMap0<K, V>
    , DefaultingMap<K, V>
    , Indexed<K, V>
{
    <C> Map<C, V> preMap (BasicFunction1<C, K> f);

    <C> Map<K, C> map (BasicFunction1<V, C> f);
}
