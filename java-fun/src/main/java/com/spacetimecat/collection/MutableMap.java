package com.spacetimecat.collection;

import java.util.NoSuchElementException;

public interface MutableMap<K, V> extends
    BasicFailMap<K, V>
    , BasicMap<K, V>
    , BasicMutableFailMap<K, V>
    , BasicMutableMap<K, V>
{
    @Override
    MutableMap<K, V> put (K k, V v);

    @Override
    BasicMutableFailMap<K, V> putOrFail (K k, V v) throws NoSuchElementException;
}
