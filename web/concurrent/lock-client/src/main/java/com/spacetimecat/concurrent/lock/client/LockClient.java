package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.BasicNamespace;
import com.spacetimecat.concurrent.lock.service.Namespace;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;

public final class LockClient implements AutoCloseable, Namespace
{
    private final LoggingResilientStoreClient store;
    private final Namespace namespace;

    /**
     * <p>
     *     A lock client that will connect to the
     *     server at the given address.
     * </p>
     *
     * @param listener
     * will be called when there are events
     *
     * @param uri
     * the address of the server such as {@code "//localhost:1234"}.
     * For more details, see {@link StoreClient#connect(String)}.
     */
    public LockClient (LockClientListener listener, String uri)
    {
        if (listener == null) { throw new NullPointerException("listener"); }
        this.store = new LoggingResilientStoreClient(uri);
        this.namespace = new MyNamespace(listener, new BasicNamespace(store));
    }

    @Override
    public Lock get (String name)
    {
        return namespace.get(name);
    }

    @Override
    public void close ()
    {
        store.close();
    }
}
