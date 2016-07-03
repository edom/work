package com.spacetimecat.collection;

final class BasicIterableFromJavaLang<A> implements BasicIterable<A>
{
    private final java.lang.Iterable<A> i;

    public BasicIterableFromJavaLang (java.lang.Iterable<A> i)
    {
        this.i = i;
    }

    @Override
    public BasicIterator<A> iterator ()
    {
        return new BasicIteratorFromJavaUtil<>(i.iterator());
    }
}
