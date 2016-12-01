package com.spacetimecat.concurrent.lock.service.store;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * <p>
 *     {@link Store} that works in the same JVM only using {@link ConcurrentHashMap}.
 * </p>
 */
public final class ConcurrentHashStore implements ListableStore
{
    private final Set<String> delegate = Collections.newSetFromMap(new ConcurrentHashMap<>(1024, 0.75F, 16));

    @Override
    public Collection<String> list ()
    {
        return new ArrayList<>(delegate);
    }

    @Override
    public boolean add (String name)
    {
        return delegate.add(name);
    }

    @Override
    public boolean remove (String name)
    {
        return delegate.remove(name);
    }
}
