package com.spacetimecat.collection;

final class BasicFiniteIteratorFlattening<A> implements BasicFiniteIterator<A>
{
    private final BasicFiniteIterator<BasicFiniteIterator<A>> bi;
    private BasicFiniteIterator<A> current;

    BasicFiniteIteratorFlattening (BasicFiniteIterator<BasicFiniteIterator<A>> bi)
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
