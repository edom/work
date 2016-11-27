package com.spacetimecat.web.http.server.imp;

import com.spacetimecat.web.http.server.Entrance;
import com.spacetimecat.web.http.server.Guest;

import java.io.IOException;
import java.io.UncheckedIOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * <p>
 *     Accept guests through a {@linkplain Socket socket}.
 * </p>
 */
public final class SocketEntrance implements Entrance
{
    private final ServerSocket server;

    public SocketEntrance (ServerSocket server)
    {
        this.server = server;
    }

    public static SocketEntrance onPort (int port)
    {
        try
        {
            final ServerSocket socket = new ServerSocket();
            socket.setReuseAddress(true);
            socket.bind(new InetSocketAddress(port));
            return new SocketEntrance(socket);
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
            server.close();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public Guest next ()
    {
        try
        {
            final Socket socket = server.accept();
            return new SocketGuest(socket);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
