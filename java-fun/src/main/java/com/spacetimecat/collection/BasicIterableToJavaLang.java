package com.spacetimecat.collection;

final class BasicIterableToJavaLang<A> implements java.lang.Iterable<A>
{
    private final BasicIterable<A> bi;

    BasicIterableToJavaLang (BasicIterable<A> bi)
    {
        this.bi = bi;
    }

    @Override
    public java.util.Iterator<A> iterator ()
    {
        return new BasicIteratorToJavaUtil<>(bi.iterator());
    }
}
