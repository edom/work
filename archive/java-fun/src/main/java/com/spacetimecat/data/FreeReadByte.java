package com.spacetimecat.data;

import com.spacetimecat.UncheckedException;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.ReadableByteChannel;

/**
 * <p>Like {@link ReadableByteChannel} but throws unchecked exceptions.</p>
 */
public class FreeReadByte
{
    ReadableByteChannel z;
    public void read (ByteBuffer b)
    {
        try
        {
            int n = z.read(b);
        }
        catch (IOException e)
        {
            throw new UncheckedException(e);
        }
    }

    static class FreeReadByte2
    {
        ByteBuffer b;
        public void read (ReadableByteChannel z)
        {
            try
            {
                int n = z.read(b);
            }
            catch (IOException e)
            {
                throw new UncheckedException(e);
            }
        }
    }
}
