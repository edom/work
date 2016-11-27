package com.spacetimecat.web.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

public final class BufferedInputStream2 extends InputStream
{
    private final InputStream input;
    private final byte[] buffer;
    private int position;
    private int limit;

    public BufferedInputStream2 (InputStream input, int bufferSize)
    {
        this.input = input;
        this.buffer = new byte[bufferSize];
    }

    @Override
    public void close ()
    {
        try
        {
            input.close();
            position = 0;
            limit = -1;
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public int read ()
    {
        if (position >= limit)
        {
            position = 0;
            limit = blockRead();
        }
        if (limit < 0) { return limit; }
        final int b = buffer[position];
        ++position;
        return b;
    }

    private int blockRead ()
    {
        try
        {
            for (;;)
            {
                final int count = input.read(buffer);
                if (count != 0) { return count; }
            }
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
