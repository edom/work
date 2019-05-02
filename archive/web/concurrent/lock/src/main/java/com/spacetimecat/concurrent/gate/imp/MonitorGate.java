package com.spacetimecat.concurrent.gate.imp;

import com.spacetimecat.concurrent.gate.Gate;

import java.util.Objects;

/**
 * <p>
 *     {@link Gate} using Java built-in monitors.
 * </p>
 */
public final class MonitorGate implements Gate
{
    private final Object lock;

    public MonitorGate (Object lock)
    {
        Objects.requireNonNull(lock, "lock");
        this.lock = lock;
    }

    /**
     * <p>
     *     Block until the mutex is free,
     *     and then run the action while holding the mutex.
     * </p>
     *
     * @param action
     * the critical section that must not be run by multiple threads
     *
     * @return
     * always true
     */
    @Override
    public boolean run (Runnable action)
    {
        synchronized (lock)
        {
            action.run();
        }
        return true;
    }
}
