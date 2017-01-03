package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.service.store.Store;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class LoggingResilientStoreClient implements Store, AutoCloseable
{
    private static final Logger logger = LoggerFactory.getLogger("com.spacetimecat.concurrent.lock.client");

    private final String uri;
    private final ResilientStoreClient delegate;

    LoggingResilientStoreClient (String uri)
    {
        this.uri = uri;
        this.delegate = new ResilientStoreClient(this::warn, uri);
    }

    @Override
    public boolean add (String name)
    {
        final boolean result = delegate.add(name);
        if (logger.isDebugEnabled())
        {
            logger.debug(String.format("%s %s acquiring %s", uri, result ? "+" : "-", name));
        }
        return result;
    }

    @Override
    public boolean remove (String name)
    {
        final boolean result = delegate.remove(name);
        if (logger.isDebugEnabled())
        {
            logger.debug(String.format("%s %s releasing %s", uri, result ? "+" : "-", name));
        }
        return result;
    }

    @Override
    public void close ()
    {
        logger.debug(String.format("%s closing", uri));
        delegate.close();
    }

    private void warn (Throwable message)
    {
        // Only print the exception class and the message; omit the stack trace.
        logger.warn(String.format("%s lock server is down: %s", uri, message));
    }
}
