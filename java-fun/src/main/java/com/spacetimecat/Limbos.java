package com.spacetimecat;

public final class Limbos
{
    private Limbos () {}

    public static MutableLimbo open ()
    {
        return new MutableLimboImpl();
    }
}
