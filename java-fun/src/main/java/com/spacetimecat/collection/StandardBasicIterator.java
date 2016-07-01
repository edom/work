package com.spacetimecat.collection;

/**
 * <p>This wraps a {@link java.util.Iterator}.</p>
 *
 * <p>The underlying Iterator cannot return null because
 * BasicIterator uses null to mean that there are no more things.</p>
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
        final A a = i.next();
        if (a == null) { throw new NullPointerException("this iterator forbids nulls"); }
        return a;
    }
}
