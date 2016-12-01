package com.spacetimecat.concurrent.lock.service.store.server;

import com.spacetimecat.concurrent.lock.service.store.ConcurrentHashStore;
import com.spacetimecat.concurrent.lock.service.store.ListableStore;
import com.spacetimecat.concurrent.lock.service.store.internal.network.smp.StoreServer;
import com.spacetimecat.server.imp.SocketEntrance;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.ServerSocket;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/**
 * <p>
 *     An implementation of Mutable String Set Protocol.
 * </p>
 */
public final class ServerMain implements Runnable, AutoCloseable
{
    private final StoreServer server;

    public ServerMain (int port)
    {
        try
        {
            final ListableStore store = new LoggingStore(new ConcurrentHashStore());
            final SocketEntrance entrance = new SocketEntrance(new ServerSocket(port));
            final ExecutorService executor = Executors.newFixedThreadPool(1024);
            this.server = new StoreServer(store, entrance, executor);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close ()
    {
        server.close();
    }

    @Override
    public void run ()
    {
        server.run();
    }

    public static void main (String[] args)
    {
        final Config config = new Config(System.getProperties());
        final int port = config.getPort();
        new ServerMain(port);
    }
}
