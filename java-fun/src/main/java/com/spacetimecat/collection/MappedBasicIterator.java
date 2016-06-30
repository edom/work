package com.spacetimecat.collection;

import com.spacetimecat.function.Function1;
import com.spacetimecat.function.Function2;
import com.spacetimecat.function.Procedure1;

final class MappedBasicIterator<A, B> implements BasicIterator<B>
{
    private final BasicIterator<A> bi;
    private final Function1<A, B> f;

    MappedBasicIterator (BasicIterator<A> bi, Function1<A, B> f)
    {
        this.bi = bi;
        this.f = f;
    }

    @Override
    public B next ()
    {
        final A a = bi.next();
        if (a == null) { return null; }
        return f.call(a);
    }

    private static <A, B> Procedure1<A> then (final Function1<A, B> f, final Procedure1<B> g)
    {
        return new Procedure1<A>()
        {
            @Override
            public void call (A a)
            {
                final B b = f.call(a);
                g.call(b);
            }
        };
    }

    private static <A, B, C> Function1<A, C> then (final Function1<A, B> f, final Function1<B, C> g)
    {
        return new Function1<A, C>()
        {
            @Override
            public C call (A a)
            {
                final B b = f.call(a);
                return g.call(b);
            }
        };
    }
}
