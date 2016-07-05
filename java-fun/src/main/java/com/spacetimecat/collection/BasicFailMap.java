package com.spacetimecat.collection;

import java.util.NoSuchElementException;

public interface BasicFailMap<K, V>
{
    V getOrFail (K k) throws NoSuchElementException;
}
