package com.spacetimecat.web.load;

public final class Content
{
    private final String type;
    private final byte[] data;

    public Content (String type, byte[] data)
    {
        this.type = type;
        this.data = data;
    }

    public String getType ()
    {
        return type;
    }

    public byte[] getData ()
    {
        return data;
    }
}
