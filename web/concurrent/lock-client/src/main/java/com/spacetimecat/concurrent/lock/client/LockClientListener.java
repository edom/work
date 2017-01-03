package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.Lock;

/**
 * <p>
 *     Receives events fired by a {@link LockClient}.
 * </p>
 *
 * <p>
 *     You create subclasses of this class.
 * </p>
 *
 * <p>
 *     Each method must not throw anything.
 * </p>
 */
public class LockClientListener
{
    /**
     * <p>
     *     Called before {@link Lock#acquire()} is called.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void enteringAcquire (String name) {}

    /**
     * <p>
     *     Called after {@link #enteringAcquire(String)} if {@link Lock#acquire()} returns false.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void acquireFailed (String name) {}

    /**
     * <p>
     *     Called after {@link #enteringAcquire(String)} if {@link Lock#acquire()} returns true.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void acquireSucceeded (String name) {}

    /**
     * <p>
     *     Called after either {@link #acquireFailed(String)}
     *     or {@link #acquireSucceeded(String)}.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void leavingAcquire (String name) {}

    /**
     * <p>
     *     Called after the JVM enters the decorator's {@link Lock#release()}.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void enteringRelease (String name) {}

    /**
     * <p>
     *     Called after {@link #enteringRelease(String)}.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param name
     * the name of the lock
     */
    protected void leavingRelease (String name) {}
}
