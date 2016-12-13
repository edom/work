package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

import com.spacetimecat.java.lang.unexceptional.*;
import com.spacetimecat.java.lang.unit.Unit;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.Socket;
import java.util.function.Supplier;

public final class ResilientSocket
{
    private final Object lock = new Object();

    private Supplier<Risky<Socket>> supplier;
    private Socket delegate;

    public ResilientSocket (Supplier<Risky<Socket>> supplier)
    {
        this.supplier = supplier;
    }

    public Risky<Socket> get ()
    {
        synchronized (lock)
        {
            if (delegate == null)
            {
                delegate = supplier.get().getValueOr(null);
            }
            return delegate == null ? new Left<>(new NullPointerException()) : new Right<>(delegate);
        }
    }

    public Risky<Integer> read (byte[] buffer)
    {
        return getInputStream()
            .then(new RiskyFunction<>(i -> i.read(buffer)));
    }

    public Risky<Unit> write (byte[] buffer)
    {
        return getOutputStream().then(new RiskyFunction<>(o ->
        {
            o.write(buffer);
            return Unit.instance;
        }));
    }

    public Risky<InputStream> getInputStream ()
    {
        return get().then(new RiskyFunction<>(Socket::getInputStream));
    }

    public Risky<OutputStream> getOutputStream ()
    {
        return get().then(new RiskyFunction<>(Socket::getOutputStream));
    }

    public Risky<Unit> close ()
    {
        synchronized (lock)
        {
            try
            {
                if (delegate != null)
                {
                    delegate.close();
                }
                return new Right<>(Unit.instance);
            }
            catch (IOException e)
            {
                return new Left<>(e);
            }
        }
    }
}
