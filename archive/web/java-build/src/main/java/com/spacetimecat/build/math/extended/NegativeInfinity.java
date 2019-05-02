package com.spacetimecat.build.math.extended;

public final class NegativeInfinity<A> extends Extended<A>
{
    @Override
    public boolean isNegativeInfinity ()
    {
        return true;
    }

    @Override
    public String toString ()
    {
        return "-Inf";
    }
}
