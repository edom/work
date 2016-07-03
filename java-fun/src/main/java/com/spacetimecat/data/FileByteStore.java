package com.spacetimecat.data;

import com.spacetimecat.UncheckedException;

import java.io.Closeable;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * Instances of this class are not thread-safe.
 */
public final class FileByteStore implements ByteStore, Closeable
{
    private final RandomAccessFile back;

    private FileByteStore (RandomAccessFile back)
    {
        this.back = back;
    }

    public static FileByteStore open (String path)
    {
        try
        {
            final RandomAccessFile f = new RandomAccessFile(path, "rws");
            return new FileByteStore(f);
        }
        catch (IOException e)
        {
            throw new UncheckedException(e);
        }
    }

    @Override
    public long read (byte[] buffer, long position, long count)
    {
        // FIXME loop
        if (count > Integer.MAX_VALUE)
        {
            throw new UnsupportedOperationException();
        }
        final int icount = (int) count;
        assert icount >= 0;
        try
        {
            back.seek(position);
            return back.read(buffer, 0, icount);
        }
        catch (IOException e)
        {
            throw new UncheckedException(e);
        }
    }

    @Override
    public void write (byte[] buffer, long position, long count)
    {
        // FIXME loop
        if (count > Integer.MAX_VALUE)
        {
            throw new UnsupportedOperationException();
        }
        final int icount = (int) count;
        assert icount >= 0;
        try
        {
            back.seek(position);
            back.write(buffer, 0, icount);
        }
        catch (IOException e)
        {
            throw new UncheckedException(e);
        }
    }

    @Override
    public void close ()
    {
        try
        {
            back.close();
        }
        catch (IOException e)
        {
            throw new UncheckedException(e);
        }
    }
}
