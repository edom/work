package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction2;

final class ScanningBasicIterator<A, B> implements BasicIterator<B>
{
    private B e;
    private final BasicFunction2<B, A, B> f;
    private final BasicIterator<A> bi;

    public ScanningBasicIterator (B e, BasicFunction2<B, A, B> f, BasicIterator<A> bi)
    {
        if (e == null) { throw new NullPointerException(); }
        this.e = e;
        this.f = f;
        this.bi = bi;
    }

    @Override
    public B next ()
    {
        if (e == null) { return null; }
        final B r = e;
        final A a = bi.next();
        e = (a != null) ? f.at(r, a) : null;
        return r;
    }
}
