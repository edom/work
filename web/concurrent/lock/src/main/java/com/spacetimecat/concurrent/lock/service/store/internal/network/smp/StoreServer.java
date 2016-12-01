package com.spacetimecat.concurrent.lock.service.store.internal.network.smp;

import com.spacetimecat.concurrent.lock.service.store.ListableStore;
import com.spacetimecat.server.imp.SocketEntrance;
import com.spacetimecat.server.imp.SocketGuest;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public final class StoreServer implements AutoCloseable, Runnable
{
    private final ListableStore delegate;
    private final SocketEntrance entrance;
    private final ExecutorService executor;

    /**
     * <p>
     *     Expose the delegate through the socket.
     * </p>
     * @param delegate
     * the backing implementation
     * @param entrance
     * accept connections
     * @param executor
     * serve connections
     */
    public StoreServer (ListableStore delegate, SocketEntrance entrance, ExecutorService executor)
    {
        this.delegate = delegate;
        this.entrance = entrance;
        this.executor = executor;
    }

    public static StoreServer open (ListableStore delegate, int port)
    {
        final ExecutorService executor = Executors.newFixedThreadPool(1024);
        return new StoreServer(delegate, SocketEntrance.onPort(port), executor);
    }

    @Override
    public void close ()
    {
        try
        (
            Close a_100 = executor::shutdownNow;
            SocketEntrance a_200 = entrance
        )
        {
        }
    }

    private interface Close extends AutoCloseable
    {
        @Override
        void close ();
    }

    @Override
    public void run ()
    {
        try (StoreServer a_100 = this)
        {
            System.out.printf("Server running at %s\n", entrance);
            for (;;)
            {
                boolean ok = false;
                final SocketGuest guest = entrance.next();
                System.out.format("Connection from %s\n", guest);
                try
                {
                    executor.submit(new StoreCompanion(delegate, guest));
                    ok = true;
                }
                catch (Throwable e)
                {
                    e.printStackTrace();
                }
                finally
                {
                    if (!ok)
                    {
                        try
                        {
                            guest.close();
                        }
                        catch (Throwable e)
                        {
                            e.printStackTrace();
                        }
                    }
                }
            }
        }
        catch (Throwable e)
        {
            e.printStackTrace();
        }
    }
}
