package com.spacetimecat.data;

import java.nio.ByteBuffer;

public interface ReadByte
{
    ReadByte read (long offset, long size, ByteBuffer target);
}
