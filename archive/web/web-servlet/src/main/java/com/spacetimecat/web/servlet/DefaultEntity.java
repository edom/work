package com.spacetimecat.web.servlet;

import java.io.IOException;
import java.io.OutputStream;
import java.nio.charset.StandardCharsets;

public final class DefaultEntity implements Entity
{
    private final String mediaType;
    private final byte[] data;

    public DefaultEntity (String mediaType, byte[] data)
    {
        this.mediaType = mediaType;
        this.data = data;
    }

    public static Entity textPlain (String text)
    {
        return new DefaultEntity("text/plain;charset=UTF-8", text.getBytes(StandardCharsets.UTF_8));
    }

    public static Entity textHtml (String html)
    {
        return new DefaultEntity("text/html;charset=UTF-8", html.getBytes(StandardCharsets.UTF_8));
    }

    @Override
    public String getMediaType ()
    {
        return mediaType;
    }

    @Override
    public void writeTo (OutputStream output) throws IOException
    {
        output.write(data);
    }
}
