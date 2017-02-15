package com.spacetimecat.web.load;

import java.io.IOException;
import java.io.InputStream;
import java.io.UncheckedIOException;

public class LoadResource implements Load
{
    private static final int capacity = 1048576;

    private final ClassLoader cls;
    private final String basePath;

    public LoadResource (ClassLoader cls, String basePath)
    {
        this.cls = cls;
        this.basePath = basePath;
    }

    @Override
    public Content load (String name)
    {
        try (HardWorking s = open(basePath + name))
        {
            final String type = LoadFileBasic.typeOf(name);
            final byte[] buffer = new byte[capacity];
            final int size = s.readAllInto(buffer);
            final byte[] data = new byte[size];
            System.arraycopy(buffer, 0, data, 0, size);
            return new Content(type, data);
        }
        catch (IOException e)
        {
            throw new UncheckedIOException(e);
        }
    }

    private HardWorking open (String name)
    {
        final InputStream s = cls.getResourceAsStream(name);
        if (s == null)
        {
            throw new NoSuchFileException(name);
        }
        return new HardWorking(s);
    }
}
