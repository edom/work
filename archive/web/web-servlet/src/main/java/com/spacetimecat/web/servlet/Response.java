package com.spacetimecat.web.servlet;

import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.util.function.Consumer;

public final class Response
{
    private final HttpServletResponse response;

    public Response (HttpServletResponse response)
    {
        this.response = response;
    }

    public void addCookie (Cookie2 cookie)
    {
        response.addCookie(cookie.encode());
    }

    public void sendOk (String contentType, Consumer<OutputStream> bodyWriter) throws IOException
    {
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType(contentType);
        bodyWriter.accept(response.getOutputStream());
    }
}
