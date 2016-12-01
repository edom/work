package com.spacetimecat.concurrent.lock.service.store.internal.network.smp;

import com.spacetimecat.concurrent.lock.service.store.Store;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.Socket;
import java.net.URI;
import java.util.Arrays;
import java.util.Collection;

public final class StoreClient implements Store, AutoCloseable
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
    public static StoreClient connect (String strUri)
    {
        try
        {
            final URI uri = URI.create(strUri);
            final String host = uri.getHost();
            final int port = uri.getPort();
            final Socket socket = new Socket(host, port);
            return new StoreClient(socket);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close ()
    {
        try
        {
            socket.close();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
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
    public boolean add (String name)
    {
        try
        {
            protocol.writeMethod(Protocol.M_ADD);
            protocol.writeStringArray(name);
            protocol.flush();
            return protocol.readBoolean();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public boolean remove (String name)
    {
        try
        {
            protocol.writeMethod(Protocol.M_REMOVE);
            protocol.writeStringArray(name);
            protocol.flush();
            return protocol.readBoolean();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public String toString ()
    {
        return String.format("StoreClient[%s]", socket);
    }
}
