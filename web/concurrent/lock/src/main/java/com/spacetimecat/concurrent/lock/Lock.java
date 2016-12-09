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
     *     Must not throw anything.
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
     *     Implementations should avoid throwing anything.
     * </p>
     *
     * <p>
     *     Implementations should allow the lock to be released
     *     by threads other than the acquiring thread.
     * </p>
     *
     * <p>
     *     If this throws an exception,
     *     the caller should assume that it no longer owns the lock.
     * </p>
     *
     * @throws LockException
     * if the implementation is reasonably certain that the client programmer misuses this object.
     * The implementation must consider other factors such as network or storage error where applicable.
     * Nevertheless, implementations should prefer returning normally even in such case.
     */
    void release ();
}
