package com.spacetimecat.concurrent.lock.service;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.store.Store;

/**
 * <p>
 *     {@link Namespace} backed by a {@link Store}.
 * </p>
 */
public class BasicNamespace implements Namespace
{
    private final Store store;

    public BasicNamespace (Store store)
    {
        this.store = store;
    }

    @Override
    public final Lock get (String name)
    {
        return new StoreLock(store, name);
    }
}
