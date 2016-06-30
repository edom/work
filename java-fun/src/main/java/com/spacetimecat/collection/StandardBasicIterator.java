package com.spacetimecat.collection;

/**
 * <p>This wraps a {@link java.util.Iterator}.</p>
 */
final class StandardBasicIterator<A> implements BasicIterator<A>
{
    private final java.util.Iterator<A> i;

    public StandardBasicIterator (java.util.Iterator<A> i)
    {
        this.i = i;
    }

    @Override
    public A next ()
    {
        if (!i.hasNext()) { return null; }
        return i.next();
    }
}
