package com.spacetimecat.concurrent.lock.service.store.server;

import com.spacetimecat.concurrent.lock.service.store.ListableStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.Collection;

final class LoggingStore implements ListableStore
{
    private static final Logger logger = LoggerFactory.getLogger("com.spacetimecat.concurrent.lock.service.store");

    private final ListableStore delegate;

    LoggingStore (ListableStore delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Collection<String> list ()
    {
        logger.info("list");
        return delegate.list();
    }

    @Override
    public boolean add (String name)
    {
        boolean result = delegate.add(name);
        logger.info(String.format("%s add %s", result ? "+" : "-", name));
        return result;
    }

    @Override
    public boolean remove (String name)
    {
        boolean result = delegate.remove(name);
        logger.info(String.format("%s remove %s", result ? "+" : "-", name));
        return result;
    }

    @Override
    public String toString ()
    {
        return delegate.toString();
    }
}
