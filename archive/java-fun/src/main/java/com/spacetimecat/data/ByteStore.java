package com.spacetimecat.data;

public interface ByteStore
{
    long read (byte[] buffer, long position, long count);

    void write (byte[] buffer, long position, long count);
}
