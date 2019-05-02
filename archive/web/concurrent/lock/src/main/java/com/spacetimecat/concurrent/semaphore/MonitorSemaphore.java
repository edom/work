package com.spacetimecat.concurrent.semaphore;

/**
 * <p>
 *     {@link Semaphore} using built-in monitors that come with Java.
 * </p>
 */
public final class MonitorSemaphore implements Semaphore
{
    private final Object lock = new Object();
    private final int limit;

    private int available;

    /**
     * <p>
     *     A counting semaphore.
     * </p>
     *
     * @param limit
     * the initial number of available units; must be positive
     */
    public MonitorSemaphore (int limit)
    {
        if (limit <= 0) { throw new IllegalArgumentException("limit <= 0"); }
        this.limit = limit;
        this.available = limit;
    }

    @Override
    public boolean acquire ()
    {
        synchronized (lock)
        {
            final boolean ok = available > 0;
            if (ok) { --available; }
            return ok;
        }
    }

    @Override
    public void release ()
    {
        synchronized (lock)
        {
            if (available < limit)
            {
                ++available;
            }
            else
            {
                throw new SemaphoreException("!(available < limit)");
            }
        }
    }
}
