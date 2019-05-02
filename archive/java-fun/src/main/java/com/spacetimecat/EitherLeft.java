package com.spacetimecat;

import com.spacetimecat.function.BasicFunction1;

final class EitherLeft<A, B> implements Either<A, B>
{
    private final A a;

    EitherLeft (A a)
    {
        this.a = a;
    }

    @Override
    public <C> C either (BasicFunction1<? super A, C> f, BasicFunction1<? super B, C> g)
    {
        return f.at(a);
    }

    @Override
    public <C, D> Either<C, D> mapBoth (BasicFunction1<? super A, C> f, BasicFunction1<? super B, D> g)
    {
        return new EitherLeft<>(f.at(a));
    }

    @Override
    public <C> Either<C, B> mapLeft (BasicFunction1<? super A, C> f)
    {
        return new EitherLeft<>(f.at(a));
    }

    @Override
    public <C> Either<A, C> mapRight (BasicFunction1<? super B, C> g)
    {
        return new EitherLeft<>(a);
    }

    @Override
    public <C> Either<A, C> bind (BasicFunction1<? super B, Either<A, C>> k)
    {
        return new EitherLeft<>(a);
    }

    @Override
    public <C> Either<A, C> map (BasicFunction1<? super B, C> f)
    {
        return mapRight(f);
    }
}
