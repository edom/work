package com.spacetimecat.concurrent.semaphore;

/**
 * <p>
 *     Counting semaphore.
 * </p>
 */
public interface Semaphore
{
    /**
     * <p>
     *     Acquire a unit.
     * </p>
     *
     * @return
     * true if we now own a unit;
     * false if the semaphore did not have any unit left to give to us.
     */
    boolean acquire ();

    /**
     * <p>
     *     Release a unit.
     * </p>
     *
     * @throws SemaphoreException
     * if this detects a {@code release} without matching {@link #acquire()}
     */
    void release ();
}
