package com.spacetimecat.build.math.extended;

public final class PositiveInfinity<A> extends Extended<A>
{
    @Override
    public boolean isPositiveInfinity ()
    {
        return true;
    }

    @Override
    public String toString ()
    {
        return "+Inf";
    }
}
