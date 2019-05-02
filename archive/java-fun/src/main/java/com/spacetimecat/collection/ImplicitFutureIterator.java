package com.spacetimecat.collection;

import com.spacetimecat.UncheckedException;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;

/**
 * <p>Only use this with {@link Future}s whose {@link Future#get() get} never returns null.</p>
 *
 * @param <A> element type
 */
final class ImplicitFutureIterator<A> implements BasicIterator<A>
{
    private final BasicIterator<Future<A>> bi;

    ImplicitFutureIterator (BasicIterator<Future<A>> bi)
    {
        this.bi = bi;
    }

    @Override
    public A next ()
    {
        final Future<A> f = bi.next();
        if (f == null) { return null; }
        try
        {
            final A a = f.get();
            if (a == null) { throw new NullPointerException(); }
            return a;
        }
        catch (InterruptedException e)
        {
            throw new UncheckedException(e);
        }
        catch (ExecutionException e)
        {
            final Throwable cause = e.getCause();
            assert cause != null;
            throw new UncheckedException(cause);
        }
    }
}
