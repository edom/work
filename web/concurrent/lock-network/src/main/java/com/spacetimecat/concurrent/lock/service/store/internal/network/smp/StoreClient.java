package com.spacetimecat.concurrent.lock.service.store.internal.network.smp;

import com.spacetimecat.concurrent.lock.service.store.RiskyStore;
import com.spacetimecat.java.lang.unexceptional.Left;
import com.spacetimecat.java.lang.unexceptional.Right;
import com.spacetimecat.java.lang.unexceptional.Risky;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.Socket;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;

public final class StoreClient implements RiskyStore, AutoCloseable
{
    private final Socket socket;
    private final Protocol protocol;

    public StoreClient (Socket socket)
    {
        this.socket = socket;
        this.protocol = Protocol.onSocket(socket);
    }

    /**
     * <p>
     *     Note that you cannot use "localhost:1234"
     *     because {@link URI} will treat "localhost" as the scheme.
     *     You need the leading double slash: "//localhost:1234".
     * </p>
     * @param strUri
     * example: "//localhost:1234"
     * @return
     * an instance
     */
    public static Risky<StoreClient> connect (String strUri)
    {
        try
        {
            final URI uri = URI.create(strUri);
            final String host = uri.getHost();
            final int port = uri.getPort();
            final Socket socket = new Socket(host, port);
            socket.setSoTimeout(1000);
            return new Right<>(new StoreClient(socket));
        }
        catch (IOException e)
        {
            return new Left<>(e);
        }
    }

    @Override
    public void close () throws IOException
    {
        socket.close();
    }

    public Collection<String> list ()
    {
        try
        {
            protocol.writeMethod(Protocol.M_LIST);
            protocol.flush();
            final String[] names = protocol.readStringArray();
            return Arrays.asList(names);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public Risky<Boolean> add (String name)
    {
        try
        {
            protocol.writeMethod(Protocol.M_ADD);
            protocol.writeStringArray(name);
            protocol.flush();
            return new Right<>(protocol.readBoolean());
        }
        catch (IOException e)
        {
            return new Left<>(e);
        }
    }

    @Override
    public Risky<Boolean> remove (String name)
    {
        try
        {
            protocol.writeMethod(Protocol.M_REMOVE);
            protocol.writeStringArray(name);
            protocol.flush();
            return new Right<>(protocol.readBoolean());
        }
        catch (IOException e)
        {
            return new Left<>(e);
        }
    }

    @Override
    public String toString ()
    {
        return String.format("StoreClient[%s]", socket);
    }
}
