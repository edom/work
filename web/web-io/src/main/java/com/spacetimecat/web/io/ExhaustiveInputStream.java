package com.spacetimecat.web.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.BufferOverflowException;
import java.nio.ByteBuffer;

/**
 * <p>
 *     An {@link InputStream} with fewer surprises.
 * </p>
 * <p>
 *     The {@linkplain #read(byte[]) read} method reads until the end of stream.
 *     {@linkplain InputStream#read(byte[]) InputStream's read} method doesn't guarantee that.
 * </p>
 */
public final class ExhaustiveInputStream implements AutoCloseable
{
    private final InputStream delegate;

    public ExhaustiveInputStream (InputStream delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public void close ()
    {
        try
        {
            delegate.close();
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * <p>
     *     Read all remaining bytes into the buffer.
     * </p>
     * @param buffer
     * target
     * @throws java.nio.BufferOverflowException
     * if the buffer is filled up before the end of stream is reached
     * @return
     * number of bytes read
     */
    public int read (byte[] buffer)
    {
        int offset = 0;
        for (;;)
        {
            final int free = buffer.length - offset;
            final int read = underlyingRead(buffer, offset, free);
            if (read < 0) { return offset; }
            if (free <= 0) { throw new BufferOverflowException(); }
            offset += read;
        }
    }

    private int underlyingRead (byte[] buffer, int offset, int length)
    {
        try
        {
            return delegate.read(buffer, offset, length);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    /**
     * <p>
     *     Read at most the given number of bytes.
     * </p>
     * @param max
     * maximum number of bytes to read
     * @return
     * an array containing the bytes read.
     * The length of the array is the number of bytes read.
     * @throws BufferOverflowException
     * if the stream is longer than {@code max}
     */
    public Result readAtMost (int max)
    {
        final byte[] buffer = new byte[max];
        final int length = read(buffer);
        final ByteBuffer byteBuffer = ByteBuffer.wrap(buffer, 0, length);
        return new Result(byteBuffer);
    }
}
