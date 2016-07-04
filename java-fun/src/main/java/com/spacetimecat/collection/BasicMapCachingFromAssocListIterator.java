package com.spacetimecat.collection;

import com.spacetimecat.function.Tuple2;

import java.util.*;
import java.util.Map;

final class BasicMapCachingFromAssocListIterator<K, V> implements BasicDumpableMap<K, V>
{
    private final BasicIterator<Tuple2<K, V>> bi;

    private final java.util.Map<K, V> cache = new HashMap<>();

    BasicMapCachingFromAssocListIterator (BasicIterator<Tuple2<K, V>> bi)
    {
        this.bi = bi;
    }

    @Override
    public V get (K k)
    {
        if (k == null) { throw new NullPointerException(); }
        final V v = cache.get(k);
        if (v != null) { return v; }
        for (;;)
        {
            final Tuple2<K, V> t = bi.next();
            if (t == null) { return null; }
            if (t.a == null) { throw new NullPointerException(); }
            if (t.b == null) { throw new NullPointerException(); }
            cache.put(t.a, t.b);
            if (k.equals(t.a)) { return t.b; }
        }
    }

    @Override
    public BasicDumpableMap<K, V> dumpTo (Map<K, V> target)
    {
        for (;;)
        {
            final Tuple2<K, V> t = bi.next();
            if (t == null) { break; }
            if (t.a == null) { throw new NullPointerException(); }
            if (t.b == null) { throw new NullPointerException(); }
            cache.put(t.a, t.b);
        }
        target.putAll(cache);
        return this;
    }

    @Override
    public Map<K, V> toNewStdMap ()
    {
        final Map<K, V> target = new HashMap<>();
        dumpTo(target);
        return target;
    }
}
