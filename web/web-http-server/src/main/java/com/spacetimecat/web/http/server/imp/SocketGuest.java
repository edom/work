package com.spacetimecat.web.http.server.imp;

import com.spacetimecat.web.http.server.Guest;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UncheckedIOException;
import java.net.Socket;

/**
 * <p>
 *     {@link Guest} that came through a {@linkplain Socket socket}.
 * </p>
 */
public final class SocketGuest implements Guest
{
    private final Socket socket;

    public SocketGuest (Socket socket)
    {
        this.socket = socket;
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

    @Override
    public String getName ()
    {
        return String.format("%s:%s", socket.getInetAddress().getHostAddress(), socket.getPort());
    }

    @Override
    public InputStream getInputStream ()
    {
        try
        {
            return socket.getInputStream();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public OutputStream getOutputStream ()
    {
        try
        {
            return socket.getOutputStream();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
