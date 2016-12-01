package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.Lock;

final class LoggingLock implements Lock
{
    private final Lock delegate;

    LoggingLock (Lock delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public boolean acquire ()
    {
        System.out.printf("%s: acquiring\n", delegate);
        final boolean ok = delegate.acquire();
        System.out.printf("%s: %s\n", delegate, ok);
        return ok;
    }

    @Override
    public void release ()
    {
        System.out.printf("%s: releasing", delegate);
        delegate.release();
    }
}
