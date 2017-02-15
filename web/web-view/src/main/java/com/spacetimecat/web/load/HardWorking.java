package com.spacetimecat.web.load;

import java.io.Closeable;
import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;
import java.nio.BufferOverflowException;

final class HardWorking implements Closeable
{
    private final InputStream self;

    HardWorking (InputStream self)
    {
        this.self = self;
    }

    int readAllInto (byte[] buffer)
    {
        final int capacity = buffer.length;
        int offset = 0;
        for (;;)
        {
            final int remain = capacity - offset;
            final int count = readPartial(buffer, offset, remain);
            if (count <= 0) { return offset; }
            if (remain <= 0) { throw new BufferOverflowException(); }
            offset += count;
        }
    }

    private int readPartial (byte[] buffer, int offset, int length)
    {
        try
        {
            return self.read(buffer, offset, length);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    @Override
    public void close () throws IOException
    {
        self.close();
    }
}
