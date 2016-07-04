package com.spacetimecat.collection;

final class BasicIteratorFlattening<A> implements BasicIterator<A>
{
    private final BasicIterator<BasicIterator<A>> bi;
    private BasicIterator<A> current;

    BasicIteratorFlattening (BasicIterator<BasicIterator<A>> bi)
    {
        this.bi = bi;
    }

    @Override
    public A next ()
    {
        if (current == null) { current = bi.next(); }
        for (;;)
        {
            if (current == null) { return null; }
            final A a = current.next();
            if (a != null) { return a; }
            current = bi.next();
        }
    }
}
