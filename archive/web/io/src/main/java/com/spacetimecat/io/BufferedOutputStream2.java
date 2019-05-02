package com.spacetimecat.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.UncheckedIOException;

/**
 * <p>
 *     Unsynchronized {@link java.io.BufferedOutputStream}.
 * </p>
 */
public final class BufferedOutputStream2 extends OutputStream
{
    private final OutputStream output;
    private final byte[] buffer;
    private int length;
    private boolean closed;

    public BufferedOutputStream2 (OutputStream output, int bufferSize)
    {
        if (bufferSize <= 0) { throw new IllegalArgumentException("bufferSize <= 0"); }
        this.output = output;
        this.buffer = new byte[bufferSize];
    }

    @Override
    public void close ()
    {
        try
        {
            drain();
            output.close();
            closed = true;
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void flush ()
    {
        try
        {
            drain();
            output.flush();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private void checkClosed ()
    {
        if (closed) { throw new UncheckedIOException(new IOException("closed")); }
    }

    @Override
    public void write (int b)
    {
        checkClosed();
        final boolean full = length >= buffer.length;
        if (full) { drain(); }
        buffer[length] = (byte) b;
        ++length;
    }

    @Override
    public void write (byte[] buffer)
    {
        write(buffer, 0, buffer.length);
    }

    @Override
    public void write (byte[] b, int off, int len)
    {
        checkClosed();
        final int newLength = length + len;
        final boolean fits = newLength <= buffer.length;
        if (fits)
        {
            System.arraycopy(b, off, buffer, length, len);
            length = newLength;
        }
        else
        {
            drain();
            directWrite(b, off, len);
        }
    }

    private void directWrite (byte[] b, int off, int len)
    {
        try
        {
            output.write(b, off, len);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private void drain ()
    {
        try
        {
            output.write(buffer, 0, length);
            length = 0;
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }
}
