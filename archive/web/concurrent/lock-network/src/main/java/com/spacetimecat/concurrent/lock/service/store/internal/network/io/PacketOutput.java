package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

import java.io.DataOutput;
import java.io.IOException;

public final class PacketOutput
{
    private final DataOutput output;

    public PacketOutput (DataOutput output)
    {
        this.output = output;
    }

    public void write (byte[] body)
    {
        try
        {
            output.writeInt(body.length);
            output.write(body);
        }
        catch (IOException e)
        {
            throw new PacketException(e);
        }
    }
}
