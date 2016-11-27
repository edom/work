package com.spacetimecat.web.http.server.imp;

import com.spacetimecat.web.http.server.Guest;
import com.spacetimecat.web.http.server.Usher;
import com.spacetimecat.web.http.server.function.HttpFunction;
import com.spacetimecat.web.http.server.log.Catcher;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

/**
 * <p>
 *     Spawn an {@link Executive} thread for each guest.
 * </p>
 */
public final class DefaultUsher implements Usher
{
    private final ExecutorService executor;
    private final Catcher catcher;
    private final HttpFunction function;

    public DefaultUsher (ExecutorService executor, Catcher catcher, HttpFunction function)
    {
        this.executor = executor;
        this.catcher = catcher;
        this.function = function;
    }

    public static DefaultUsher create (int threads, HttpFunction function)
    {
        final ThreadGroup parent = Thread.currentThread().getThreadGroup();
        final ThreadGroup group = new ThreadGroup(parent, "server");
        final ThreadFactory factory = r -> new Thread(group, r);
        final ExecutorService executor = Executors.newFixedThreadPool(threads, factory);
        return new DefaultUsher(executor, Throwable::printStackTrace, function);
    }

    @Override
    public void close ()
    {
        executor.shutdown();
        try
        {
            executor.awaitTermination(3L, TimeUnit.SECONDS);
        }
        catch (InterruptedException ignored)
        {
        }
    }

    @Override
    public void handle (Guest guest)
    {
        try
        {
            final Executive executive = new Executive(guest, function);
            executor.execute(executive);
        }
        catch (Throwable e)
        {
            catcher.accept(e);
        }
        finally
        {
            try
            {
                guest.close();
            }
            catch (Throwable e)
            {
                catcher.accept(e);
            }
        }
    }
}
