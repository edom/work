package com.spacetimecat.data;

import java.util.Map;

public class MemoryKeyValueStore implements KeyValueStore
{
    private final Map<byte[], byte[]> backing;

    public MemoryKeyValueStore (Map<byte[], byte[]> backing)
    {
        this.backing = backing;
    }

    @Override
    public byte[] read (byte[] key)
    {
        return backing.get(key);
    }

    @Override
    public void write (byte[] key, byte[] value)
    {
        backing.put(key, value);
    }
}
