package com.spacetimecat;

import com.spacetimecat.function.BasicCheckedFunction1;
import com.spacetimecat.function.BasicCheckedProcedure1;

public final class Limbos
{
    private Limbos () {}

    public static Limbo open ()
    {
        return new LimboImpl();
    }

    public static <R> R with (BasicCheckedFunction1<? super Limbo, R> f)
    {
        final Limbo limbo = open();
        try
        {
            final R r = f.at(limbo);
            limbo.close();
            return r;
        }
        catch (Throwable t)
        {
            return limbo.closeAndThrowUnchecked(t);
        }
    }

    public static Limbo with_ (BasicCheckedProcedure1<? super Limbo> f)
    {
        final Limbo limbo = open();
        try
        {
            f.call(limbo);
            limbo.close();
            return limbo;
        }
        catch (Throwable t)
        {
            return limbo.closeAndThrowUnchecked(t);
        }
    }
}
