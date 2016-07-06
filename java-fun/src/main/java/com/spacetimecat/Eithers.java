package com.spacetimecat;

public final class Eithers
{
    private Eithers () {}

    public static <A, B> Either<A, B> Left (A a)
    {
        return new EitherLeft<>(a);
    }

    public static <A, B> Either<A, B> Right (B b)
    {
        return new EitherRight<>(b);
    }
}
