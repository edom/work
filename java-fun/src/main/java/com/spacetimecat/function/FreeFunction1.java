package com.spacetimecat.function;

final class FreeFunction1<A, B> implements Function1<A, B>
{
    private final BasicFunction1<A, B> f;

    FreeFunction1 (BasicFunction1<A, B> f)
    {
        this.f = f;
    }

    @Override
    public <C> Function1<C, B> after (BasicFunction1<C, A> that)
    {
        return new FreeFunction1<>(new ComposedBasicFunction1<>(this, that));
    }

    @Override
    public <C> Function1<A, C> then (BasicFunction1<B, C> that)
    {
        return new FreeFunction1<>(new ComposedBasicFunction1<>(that, this));
    }

    @Override
    public B at (A a)
    {
        return f.at(a);
    }
}
