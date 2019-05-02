package com.spacetimecat.concurrent.gate.imp;

import com.spacetimecat.concurrent.gate.Gate;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * <p>
 *     {@link Gate} using {@link AtomicBoolean}.
 * </p>
 */
public final class AtomicBooleanGate implements Gate
{
    private final AtomicBoolean lock;

    public AtomicBooleanGate (AtomicBoolean lock)
    {
        Objects.requireNonNull(lock, "lock");
        this.lock = lock;
    }

    @Override
    public boolean run (Runnable action)
    {
        final boolean ourTurn = lock.compareAndSet(false, true);
        if (ourTurn)
        {
            try
            {
                action.run();
            }
            finally
            {
                lock.set(false);
            }
        }
        return ourTurn;
    }
}
