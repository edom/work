package com.spacetimecat.data;

import java.nio.channels.ReadableByteChannel;

public interface Buffer
{
    Buffer read (ReadableByteChannel c);
}
