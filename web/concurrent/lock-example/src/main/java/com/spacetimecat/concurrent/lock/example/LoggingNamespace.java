package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.Namespace;

final class LoggingNamespace implements Namespace
{
    private final Namespace delegate;

    LoggingNamespace (Namespace delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Lock get (String name)
    {
        return new LoggingLock(delegate.get(name));
    }
}
