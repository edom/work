package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     A piece of code that must not be run by multiple processes.
 * </p>
 */
public final class CriticalSection
{
    private final Runnable action;

    public CriticalSection (Runnable action)
    {
        this.action = action;
    }

    public boolean runWith (Lock lock)
    {
        return new WithLock(lock).run(action);
    }
}
