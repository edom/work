package com.spacetimecat.collection;

import java.util.Map;

final class BasicMap0FromJavaUtil<K, V> implements BasicMap0<K, V>
{
    private final java.util.Map<K, V> m;

    BasicMap0FromJavaUtil (Map<K, V> m)
    {
        this.m = m;
    }

    @Override
    public V get (K k)
    {
        return m.get(k);
    }

    @Override
    public Iterable<K> keys ()
    {
        return Iterables.from(m.keySet());
    }

    @Override
    public Iterable<V> values ()
    {
        return Iterables.from(m.values());
    }
}
