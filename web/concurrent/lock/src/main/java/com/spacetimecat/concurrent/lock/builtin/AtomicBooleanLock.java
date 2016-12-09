package com.spacetimecat.concurrent.lock.builtin;

import com.spacetimecat.concurrent.lock.Lock;

import java.util.concurrent.atomic.AtomicBoolean;

public final class AtomicBooleanLock implements Lock
{
    private final AtomicBoolean lock = new AtomicBoolean();

    @Override
    public boolean acquire ()
    {
        return lock.compareAndSet(false, true);
    }

    @Override
    public void release ()
    {
        lock.set(false);
    }
}
