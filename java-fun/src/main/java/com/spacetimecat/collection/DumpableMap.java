package com.spacetimecat.collection;

public interface DumpableMap<K, V>
{
    DumpableMap<K, V> dumpTo (java.util.Map<K, V> target);
}
