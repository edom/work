package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.service.store.Store;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;
import com.spacetimecat.java.lang.resilient.Resilient;

import java.util.function.Consumer;

/**
 * <p>
 *     {@link StoreClient} adapted for locks.
 * </p>
 */
final class ResilientStoreClient implements Store, AutoCloseable
{
    private final Consumer<Throwable> warn;
    private final Resilient<StoreClient> resilient;

    ResilientStoreClient (Consumer<Throwable> warn, String uri)
    {
        if (warn == null) { throw new NullPointerException("warn"); }
        this.warn = warn;
        this.resilient = new Resilient<>(new StoreHeaven(uri));
    }

    @Override
    public boolean add (String name)
    {
        return resilient.obtain()
            .then(instance ->
                instance.add(name)
                    .ifFailRun(() -> kill(instance))
        ).fold(throwable -> { warn.accept(throwable); return false; }, bool -> bool);
    }

    @Override
    public boolean remove (String name)
    {
        return resilient.obtain()
            .then(instance ->
                instance.remove(name)
                    .ifFailRun(() -> kill(instance))
        ).fold(throwable -> { warn.accept(throwable); return false; }, bool -> bool);
    }

    private void kill (StoreClient instance)
    {
        // We can ignore errors here because the next call to obtain should fix it.
        resilient.kill(instance).ifFailDo(warn);
    }

    @Override
    public void close ()
    {
        resilient.close();
    }
}
