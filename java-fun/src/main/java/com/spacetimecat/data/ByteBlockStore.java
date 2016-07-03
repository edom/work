package com.spacetimecat.data;

import com.spacetimecat.UncheckedException;

import java.io.Closeable;
import java.io.IOException;

public class ByteBlockStore implements BlockStore, Closeable
{
    private final long blockSize;
    private final ByteStore backing;

    public ByteBlockStore (long blockSize, ByteStore backing)
    {
        if (blockSize > Integer.MAX_VALUE)
        {
            // XXX
            throw new UnsupportedOperationException();
        }
        this.blockSize = blockSize;
        this.backing = backing;
    }

    @Override
    public byte[] read (long index)
    {
        final int irecordSize = (int) blockSize;
        final byte[] buffer = new byte[irecordSize];
        if (backing.read(buffer, blockSize * index, blockSize) != blockSize)
        {
            // FIXME
            throw new RuntimeException("read failure");
        }
        return buffer;
    }

    @Override
    public void write (long index, byte[] bytes)
    {
        backing.write(bytes, blockSize * index, blockSize);
    }

    @Override
    public void close ()
    {
        // FIXME
        if (backing instanceof Closeable)
        {
            try
            {
                ((Closeable) backing).close();
            }
            catch (IOException e)
            {
                throw new UncheckedException(e);
            }
        }
    }
}
