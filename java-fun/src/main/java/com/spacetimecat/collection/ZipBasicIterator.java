package com.spacetimecat.collection;

import com.spacetimecat.function.Function2;

public final class ZipBasicIterator<A, B, C> implements BasicIterator<C>
{
    private final BasicIterator<A> as;
    private final BasicIterator<B> bs;
    private final Function2<A, B, C> f;

    public ZipBasicIterator (BasicIterator<A> as, BasicIterator<B> bs, Function2<A, B, C> f)
    {
        this.as = as;
        this.bs = bs;
        this.f = f;
    }

    @Override
    public C next ()
    {
        final A a = as.next();
        if (a == null) { return null; }
        final B b = bs.next();
        if (b == null) { return null; }
        return f.call(a, b);
    }
}
