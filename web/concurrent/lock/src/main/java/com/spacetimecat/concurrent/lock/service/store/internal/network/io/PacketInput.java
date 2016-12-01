package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

import java.io.DataInput;
import java.io.EOFException;
import java.io.IOException;

public final class PacketInput
{
    private final int limit;
    private final DataInput input;

    public PacketInput (int limit, DataInput input)
    {
        this.limit = limit;
        this.input = input;
    }

    public byte[] read ()
    {
        try
        {
            final int length = input.readInt();
            if (length < 0) { throw new FormatException("negative length"); }
            if (length > limit) { throw new LimitException(String.format("%s > %s", length, limit)); }
            final byte[] body = new byte[length];
            input.readFully(body);
            return body;
        }
        catch (EOFException e)
        {
            throw new EofException(e);
        }
        catch (IOException e)
        {
            throw new PacketException(e);
        }
    }
}
