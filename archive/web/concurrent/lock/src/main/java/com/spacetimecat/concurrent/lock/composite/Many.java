package com.spacetimecat.concurrent.lock.composite;

import com.spacetimecat.concurrent.lock.Lock;

import java.util.Arrays;

/**
 * <p>
 *     {@link Lock} made of an arbitrary sequence of locks.
 * </p>
 */
public final class Many implements Lock
{
    private final Lock delegate;

    private Many (Lock delegate)
    {
        this.delegate = delegate;
    }

    public static Many of (Iterable<? extends Lock> locks)
    {
        Lock result = new Zero();
        for (Lock lock : locks)
        {
            result = new Two(result, lock);
        }
        return new Many(result);
    }

    public static Many of (Lock... locks)
    {
        return of(Arrays.asList(locks));
    }

    @Override
    public boolean acquire ()
    {
        return delegate.acquire();
    }

    @Override
    public void release ()
    {
        delegate.release();
    }
}
