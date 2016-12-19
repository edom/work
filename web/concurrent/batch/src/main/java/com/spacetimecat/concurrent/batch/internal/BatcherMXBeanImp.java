package com.spacetimecat.concurrent.batch.internal;

import java.time.Instant;

/**
 * <p>
 *     This should be the only implementation of {@link BatcherMXBean}.
 * </p>
 */
public final class BatcherMXBeanImp implements BatcherMXBean
{
    private final BatchByTime<?> managee;

    public BatcherMXBeanImp (BatchByTime<?> managee)
    {
        this.managee = managee;
    }

    @Override
    public long getDelay ()
    {
        synchronized (managee.lock)
        {
            return managee.delay;
        }
    }

    @Override
    public void setDelay (long delay)
    {
        if (delay < 0L) { throw new IllegalArgumentException("delay < 0L"); }
        if (delay > 1000L) { throw new IllegalArgumentException("delay > 1000L"); }
        synchronized (managee.lock)
        {
            managee.delay = delay;
        }
    }

    @Override
    public int getSize ()
    {
        synchronized (managee.lock)
        {
            return managee.batch.size();
        }
    }

    @Override
    public long getCounterStartMilliEpoch ()
    {
        synchronized (managee.lock)
        {
            return managee.counterStartEpochMilli;
        }
    }

    @Override
    public String getCounterStartTime ()
    {
        return Instant.ofEpochMilli(managee.counterStartEpochMilli).toString();
    }

    @Override
    public int getAcceptCount ()
    {
        synchronized (managee.lock)
        {
            return managee.acceptCount;
        }
    }

    @Override
    public int getRejectCount ()
    {
        synchronized (managee.lock)
        {
            return managee.rejectCount;
        }
    }

    @Override
    public int getRequestCount ()
    {
        synchronized (managee.lock)
        {
            return managee.acceptCount + managee.rejectCount;
        }
    }

    @Override
    public void resetCounters ()
    {
        synchronized (managee.lock)
        {
            managee.acceptCount = 0;
            managee.rejectCount = 0;
            managee.counterStartEpochMilli = System.currentTimeMillis();
        }
    }

    @Override
    public double getAverageRequestPerSecond ()
    {
        final int requestCount;
        final long counterStartEpochMilli;
        synchronized (managee.lock)
        {
            requestCount = managee.acceptCount + managee.rejectCount;
            counterStartEpochMilli = managee.counterStartEpochMilli;
        }
        if (requestCount < 0)
        {
            // Arithmetic overflow.
            return Double.POSITIVE_INFINITY;
        }
        final long now = System.currentTimeMillis();
        final long deltaMilli = now - counterStartEpochMilli;
        if (deltaMilli <= 0L)
        {
            return Double.NEGATIVE_INFINITY;
        }
        return 1000 * ((double) requestCount / deltaMilli);
    }
}
