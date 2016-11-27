package com.spacetimecat.web.servlet.logic;

import com.spacetimecat.web.http.MediaType;
import com.spacetimecat.web.servlet.Request;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

public final class Static implements Logic
{
    private final MediaType mediaType;
    private final byte[] body;

    public Static (MediaType mediaType, byte[] body)
    {
        this.mediaType = mediaType;
        this.body = body;
    }

    public static Static plain (String text)
    {
        final MediaType mediaType = MediaType.parse("text/plain;charset=UTF-8");
        final byte[] body = text.getBytes(StandardCharsets.UTF_8);
        return new Static(mediaType, body);
    }

    public static Static html (String html)
    {
        final MediaType mediaType = MediaType.parse("text/html;charset=UTF-8");
        final byte[] body = html.getBytes(StandardCharsets.UTF_8);
        return new Static(mediaType, body);
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        if (new Request(request).negotiation().offer(mediaType).isAccepted())
        {
            response.setContentType(mediaType.unparsed());
            response.getOutputStream().write(body);
            return true;
        }
        return false;
    }
}
