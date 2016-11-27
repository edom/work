package com.spacetimecat.web.http.syntax;

import com.spacetimecat.web.io.EmptyInputStream;

import java.io.InputStream;

public final class HttpRequest
{
    public RequestHeader header;
    public InputStream body;

    public boolean isValid ()
    {
        return header.valid;
    }

    public void setValid (boolean valid)
    {
        header.valid = valid;
    }

    public String getVersionOr (String fallback)
    {
        return isValid() ? header.requestLine.version : fallback;
    }

    public void setMethod (String method)
    {
        header.requestLine.method = method;
    }

    public void setUri (String uri)
    {
        header.requestLine.uri = uri;
    }

    public void setVersion (String version)
    {
        header.requestLine.version = version;
    }

    public String getRawRequestLine ()
    {
        return header.requestLine.unparsed;
    }

    public static HttpRequest get (String uri, String version)
    {
        final HttpRequest x = new HttpRequest();
        x.header = RequestHeader.get(uri, version);
        x.body = new EmptyInputStream();
        return x;
    }

    public String getMethod ()
    {
        return header.requestLine.method;
    }

    public String getUri ()
    {
        return header.requestLine.uri;
    }

    public String getVersion ()
    {
        return header.requestLine.version;
    }
}
