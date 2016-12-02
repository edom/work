package com.spacetimecat.build.math.extended;

public final class Normal<A> extends Extended<A>
{
    private final A value;

    public Normal (A value)
    {
        this.value = value;
    }

    @Override
    public A value ()
    {
        return value;
    }

    @Override
    public String toString ()
    {
        return value.toString();
    }
}
