package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     Binary semaphore.
 * </p>
 * <p>
 *     This is smaller than {@link java.util.concurrent.locks.Lock}
 *     that comes with the Java runtime library
 *     so this should be easier to implement.
 * </p>
 */
public interface Lock
{
    /**
     * <p>
     *     Acquire the lock.
     * </p>
     * @return
     * true iff the caller now owns the lock,
     * and the lock becomes engaged
     */
    boolean acquire ();

    /**
     * <p>
     *     Release the lock.
     * </p>
     * <p>
     *     Implementations may limit who can release the lock.
     * </p>
     * <p>
     *     Implementations may detect calls that are not paired with {@link #acquire()}.
     * </p>
     * @throws LockException
     * if the lock is not engaged.
     */
    void release ();
}
