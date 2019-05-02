package com.spacetimecat.collection;

public final class Mutables
{
    private Mutables () {}

    public static <A> BasicMutableCollection<A> from (java.util.Collection<A> c)
    {
        return new BasicMutableCollectionFromJavaUtil<>(c);
    }
}
