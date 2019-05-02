package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.Namespace;

final class MyNamespace implements Namespace
{
    private final LockClientListener listener;
    private final Namespace delegate;

    MyNamespace (LockClientListener listener, Namespace delegate)
    {
        this.listener = listener;
        this.delegate = delegate;
    }

    @Override
    public Lock get (String name)
    {
        return new MyLock(listener, name, delegate.get(name));
    }
}
