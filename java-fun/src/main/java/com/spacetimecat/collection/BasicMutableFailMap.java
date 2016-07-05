package com.spacetimecat.collection;

import java.util.NoSuchElementException;

public interface BasicMutableFailMap<K, V>
{
    BasicMutableFailMap<K, V> putOrFail (K k, V v) throws NoSuchElementException;
}
