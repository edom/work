package com.spacetimecat.collection;

public final class Maps
{
    private Maps () {}
    public static <K, V> Map<K, V> from (java.util.Map<K, V> m)
    {
        if (m == null) { throw new NullPointerException(); }
        throw new UnsupportedOperationException(); // FIXME
//        return new BasicMap0FromJavaUtil<>(m);
    }
}
