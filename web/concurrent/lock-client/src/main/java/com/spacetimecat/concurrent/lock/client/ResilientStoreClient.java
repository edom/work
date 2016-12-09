package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.service.store.Store;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;
import com.spacetimecat.java.lang.resilient.Resilient;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 *     {@link StoreClient} adapted for locks.
 * </p>
 */
final class ResilientStoreClient implements Store, AutoCloseable
{
    private static final Logger logger = LoggerFactory.getLogger("com.spacetimecat.concurrent.lock.client");

    private final String uri;
    private final Resilient<StoreClient> resilient;

    ResilientStoreClient (String uri)
    {
        this.uri = uri;
        this.resilient = new Resilient<>(new StoreHeaven(uri));
    }

    private void warn (Object message)
    {
        // Only print the exception class and the message; omit the stack trace.
        logger.warn(String.format("%s lock server is down: %s", uri, message));
    }

    @Override
    public boolean add (String name)
    {
        boolean result = resilient.obtain()
            .then(instance ->
                instance.add(name)
                    .ifFailRun(() -> kill(instance))
        ).fold(bool -> bool, throwable -> { warn(throwable); return false; });
        logger.info(String.format("%s %s acquiring %s", uri, result ? "+" : "-", name));
        return result;
    }

    @Override
    public boolean remove (String name)
    {
        boolean result = resilient.obtain()
            .then(instance ->
                instance.remove(name)
                    .ifFailRun(() -> kill(instance))
        ).fold(bool -> bool, throwable -> { warn(throwable); return false; });
        logger.info(String.format("%s %s releasing %s", uri, result ? "+" : "-", name));
        return result;
    }

    private void kill (StoreClient instance)
    {
        // We can ignore errors here because the next call to obtain should fix it.
        resilient.kill(instance).ifFailDo(this::warn);
    }

    @Override
    public void close ()
    {
        resilient.close();
    }

}
