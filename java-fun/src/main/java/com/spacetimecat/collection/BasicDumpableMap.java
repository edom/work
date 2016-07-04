package com.spacetimecat.collection;

import java.util.Map;

public interface BasicDumpableMap<K, V> extends
    BasicMap<K, V>
    , DumpableMap<K, V>
    , ToNewStdMap<K, V>
{
    @Override
    BasicDumpableMap<K, V> dumpTo (Map<K, V> target);
}
