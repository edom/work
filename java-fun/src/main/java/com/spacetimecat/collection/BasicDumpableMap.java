package com.spacetimecat.collection;

import java.util.*;
import java.util.Map;

public interface BasicDumpableMap<K, V> extends
    BasicMap<K, V>
    , DumpableMap<K, V>
{
    @Override
    BasicDumpableMap<K, V> dumpTo (Map<K, V> target);
}
