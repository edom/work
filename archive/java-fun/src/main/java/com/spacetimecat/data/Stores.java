package com.spacetimecat.data;

import java.io.File;
import java.nio.channels.FileChannel;

public class Stores
{
    public static FileByteStore file (String path)
    {
        return FileByteStore.open(path);
    }

    public static ByteBlockStore block (long blockSize, ByteStore backing)
    {
        FileChannel z;
        return new ByteBlockStore(blockSize, backing);
    }
}
