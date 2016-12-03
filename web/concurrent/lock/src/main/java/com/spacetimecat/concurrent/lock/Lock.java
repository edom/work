package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     Binary semaphore.
 * </p>
 *
 * <p>
 *     This is smaller than {@link java.util.concurrent.locks.Lock}
 *     that comes with the Java runtime library
 *     so this should be easier to implement.
 * </p>
 *
 * <p>
 *     The lock may reside in another machine.
 *     For example, an implementation may be a remote proxy
 *     that forwards the method over the network.
 * </p>
 */
public interface Lock
{
    /**
     * <p>
     *     Acquire the lock.
     * </p>
     *
     * <p>
     *     If there is an underlying input-output error,
     *     this should throw an exception.
     *     The caller should log the exception
     *     and retry locking after a few seconds.
     * </p>
     *
     * @return
     * true iff the caller now owns the lock,
     * and the lock becomes engaged
     */
    boolean acquire ();

    /**
     * <p>
     *     Release the lock.
     * </p>
     *
     * <p>
     *     Implementations may limit who can release the lock.
     * </p>
     *
     * <p>
     *     Implementations may detect calls that are not paired with {@link #acquire()}.
     * </p>
     *
     * <p>
     *     If this throws an exception,
     *     the caller should assume that it no longer owns the lock.
     * </p>
     *
     * @throws LockException
     * if the lock is not engaged.
     */
    void release ();
}
