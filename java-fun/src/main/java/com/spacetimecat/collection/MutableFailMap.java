package com.spacetimecat.collection;

import java.util.NoSuchElementException;

public interface MutableFailMap<K, V> extends
    BasicFailMap<K, V>
    , BasicMutableFailMap<K, V>
{
    @Override
    MutableFailMap<K, V> putOrFail (K k, V v) throws NoSuchElementException;
}
