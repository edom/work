package com.spacetimecat.web.http.syntax;

import com.spacetimecat.web.io.EmptyInputStream;

import java.io.InputStream;

public final class HttpResponse
{
    public ResponseHeader header;
    public InputStream body;

    public static HttpResponse ok (String version)
    {
        final HttpResponse x = new HttpResponse();
        x.header = ResponseHeader.ok(version);
        x.body = new EmptyInputStream();
        return x;
    }

    public static HttpResponse noContent (String version)
    {
        final HttpResponse x = new HttpResponse();
        x.header = ResponseHeader.noContent(version);
        x.body = new EmptyInputStream();
        return x;
    }

    public static HttpResponse badRequest (String version)
    {
        final HttpResponse x = new HttpResponse();
        x.header = ResponseHeader.badRequest(version);
        x.body = new EmptyInputStream();
        return x;
    }
}
