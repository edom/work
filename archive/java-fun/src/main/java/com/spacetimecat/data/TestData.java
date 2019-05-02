package com.spacetimecat.data;

import java.io.ByteArrayOutputStream;
import java.io.DataOutputStream;
import java.io.ObjectOutputStream;
import java.nio.ByteBuffer;
import java.util.Arrays;

public class TestData
{
    public static void main (String[] args) throws Exception
    {
        try (final FileByteStore fs = Stores.file("/tmp/testdata"))
        {
            final int blockSize = 4096;
            final BlockStore as = Stores.block(blockSize, fs);
            final ByteArrayOutputStream bao = new ByteArrayOutputStream();
            final ObjectOutputStream z = new ObjectOutputStream(bao);
            z.writeLong(0xdeadbeefbaadf00dL);
            z.flush();
            final ByteBuffer buf = ByteBuffer.allocateDirect(blockSize);
            buf.put(bao.toByteArray());
//      as.write(0, );
            final byte[] b0 = as.read(0);
            System.out.println(b0);
        }
    }
}
