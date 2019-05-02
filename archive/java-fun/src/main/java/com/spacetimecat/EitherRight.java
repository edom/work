package com.spacetimecat;

import com.spacetimecat.function.BasicFunction1;

final class EitherRight<A, B> implements Either<A, B>
{
    private final B b;

    EitherRight (B b)
    {
        this.b = b;
    }

    @Override
    public <C> C either (BasicFunction1<? super A, C> f, BasicFunction1<? super B, C> g)
    {
        return g.at(b);
    }

    @Override
    public <C, D> Either<C, D> mapBoth (BasicFunction1<? super A, C> f, BasicFunction1<? super B, D> g)
    {
        return new EitherRight<>(g.at(b));
    }

    @Override
    public <C> Either<C, B> mapLeft (BasicFunction1<? super A, C> f)
    {
        return new EitherRight<>(b);
    }

    @Override
    public <C> Either<A, C> mapRight (BasicFunction1<? super B, C> g)
    {
        return new EitherRight<>(g.at(b));
    }

    @Override
    public <C> Either<A, C> bind (BasicFunction1<? super B, Either<A, C>> k)
    {
        return k.at(b);
    }

    @Override
    public <C> Either<A, C> map (BasicFunction1<? super B, C> f)
    {
        return mapRight(f);
    }
}
