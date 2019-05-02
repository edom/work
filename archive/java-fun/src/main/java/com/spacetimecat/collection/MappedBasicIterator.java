package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicProcedure1;

final class MappedBasicIterator<A, B> implements BasicIterator<B>
{
    private final BasicIterator<A> bi;
    private final BasicFunction1<? super A, B> f;

    MappedBasicIterator (BasicIterator<A> bi, BasicFunction1<? super A, B> f)
    {
        this.bi = bi;
        this.f = f;
    }

    @Override
    public B next ()
    {
        final A a = bi.next();
        if (a == null) { return null; }
        return f.at(a);
    }

    private static <A, B> BasicProcedure1<A> then (final BasicFunction1<A, B> f, final BasicProcedure1<B> g)
    {
        return new BasicProcedure1<A>()
        {
            @Override
            public void call (A a)
            {
                final B b = f.at(a);
                g.call(b);
            }
        };
    }

    private static <A, B, C> BasicFunction1<A, C> then (final BasicFunction1<A, B> f, final BasicFunction1<B, C> g)
    {
        return new BasicFunction1<A, C>()
        {
            @Override
            public C at (A a)
            {
                final B b = f.at(a);
                return g.at(b);
            }
        };
    }
}
