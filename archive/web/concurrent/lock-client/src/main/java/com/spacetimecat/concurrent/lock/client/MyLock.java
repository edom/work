package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.Lock;

final class MyLock implements Lock
{
    private final LockClientListener listener;
    private final String name;
    private final Lock delegate;

    MyLock (LockClientListener listener, String name, Lock delegate)
    {
        this.listener = listener;
        this.name = name;
        this.delegate = delegate;
    }

    @Override
    public boolean acquire ()
    {
        listener.enteringAcquire(name);
        final boolean value = delegate.acquire();
        if (value)      { listener.acquireSucceeded(name); }
        else            { listener.acquireFailed(name); }
        listener.leavingAcquire(name);
        return value;
    }

    @Override
    public void release ()
    {
        listener.enteringRelease(name);
        delegate.release();
        listener.leavingRelease(name);
    }
}
