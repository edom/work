package com.spacetimecat.function;

import com.spacetimecat.UncheckedException;

import java.util.concurrent.Callable;

final class BasicFunction0FromCallable<A> implements BasicFunction0<A>
{
    private final Callable<A> c;

    BasicFunction0FromCallable (Callable<A> c)
    {
        this.c = c;
    }

    @Override
    public A at ()
    {
        try
        {
            return c.call();
        }
        catch (RuntimeException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new UncheckedException(e);
        }
    }
}
