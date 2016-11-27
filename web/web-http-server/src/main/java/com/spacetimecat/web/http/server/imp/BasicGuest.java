package com.spacetimecat.web.http.server.imp;

import com.spacetimecat.web.http.server.Guest;
import com.spacetimecat.web.io.CloseAll;

import java.io.InputStream;
import java.io.OutputStream;

public final class BasicGuest implements Guest
{
    private final String name;
    private final InputStream input;
    private final OutputStream output;

    public BasicGuest (String name, InputStream input, OutputStream output)
    {
        this.name = name;
        this.input = input;
        this.output = output;
    }

    @Override
    public void close ()
    {
        CloseAll.of(input, output).close();
    }

    @Override
    public String getName ()
    {
        return name;
    }

    @Override
    public InputStream getInputStream ()
    {
        return input;
    }

    @Override
    public OutputStream getOutputStream ()
    {
        return output;
    }
}
