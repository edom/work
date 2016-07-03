package com.spacetimecat.function;

final class ComposedBasicFunction1<A, B, C> implements BasicFunction1<A, C>
{
    private final BasicFunction1<B, C> f;
    private final BasicFunction1<A, B> g;

    ComposedBasicFunction1 (BasicFunction1<B, C> f, BasicFunction1<A, B> g)
    {
        this.f = f;
        this.g = g;
    }

    @Override
    public C at (A a)
    {
        final B b = g.at(a);
        return f.at(b);
    }
}
