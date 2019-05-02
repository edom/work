package com.spacetimecat.concurrent.lock;

import com.spacetimecat.java.lang.Try;

/**
 * <p>
 *     Control construct that acquires and releases lock.
 * </p>
 */
public final class WithLock
{
    private final Lock lock;

    public WithLock (Lock lock)
    {
        this.lock = lock;
    }

    public boolean run (Runnable action)
    {
        if (lock.acquire())
        {
            new Try(action).andFinally(lock::release);
            return true;
        }
        else
        {
            return false;
        }
    }
}
