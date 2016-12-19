package com.spacetimecat.concurrent.batch.internal;

/**
 * <p>
 *     Experimental;
 *     for use with Java Management Extensions
 *     and the {@code jconsole} application.
 * </p>
 */
public interface BatcherMXBean
{
    long getDelay ();

    void setDelay (long delay);

    int getSize ();

    long getCounterStartMilliEpoch ();

    String getCounterStartTime ();

    int getAcceptCount ();

    int getRejectCount ();

    int getRequestCount ();

    double getAverageRequestPerSecond ();

    void resetCounters ();
}
