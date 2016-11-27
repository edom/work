package com.spacetimecat.web.io;

import java.io.InputStream;

public final class EmptyInputStream extends InputStream
{
    @Override
    public int read ()
    {
        return -1;
    }
}
