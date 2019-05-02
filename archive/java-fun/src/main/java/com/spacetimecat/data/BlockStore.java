package com.spacetimecat.data;

public interface BlockStore
{
    byte[] read (long index);

    void write (long index, byte[] bytes);
}
