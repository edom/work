package com.spacetimecat.concurrent.lock.client;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.service.BasicNamespace;
import com.spacetimecat.concurrent.lock.service.Namespace;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreClient;

public final class LockClient implements AutoCloseable, Namespace
{
    private final ResilientStoreClient store;
    private final Namespace delegate;
    /**
     * <p>
     *     A lock client that will connect to the
     *     server at the given address.
     * </p>
     * @param uri
     * the address of the server such as {@code "//localhost:1234"}.
     * For more details, see {@link StoreClient#connect(String)}.
     */
    public LockClient (String uri)
    {
        store = new ResilientStoreClient(uri);
        delegate = new BasicNamespace(store);
    }

    @Override
    public Lock get (String name)
    {
        return delegate.get(name);
    }

    @Override
    public void close ()
    {
        store.close();
    }
}
