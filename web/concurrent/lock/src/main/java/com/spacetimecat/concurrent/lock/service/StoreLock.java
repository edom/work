package com.spacetimecat.concurrent.lock.service;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.LockException;
import com.spacetimecat.concurrent.lock.service.store.Store;

/**
 * <p>
 *     {@link Lock} backed by a {@link Store}.
 * </p>
 */
public final class StoreLock implements Lock
{
    private final Store source;
    private final String name;

    public StoreLock (Store source, String name)
    {
        this.source = source;
        this.name = name;
    }

    @Override
    public boolean acquire ()
    {
        return source.add(name);
    }

    public void release ()
    {
        if (!source.remove(name))
        {
            throw new LockException(toString());
        }
    }

    @Override
    public String toString ()
    {
        return String.format("%s %s", source, name);
    }
}
