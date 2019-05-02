package com.spacetimecat.web.http.syntax;

import java.util.ArrayList;
import java.util.Collection;

public final class RequestHeader
{
    public boolean valid;
    public RequestLine requestLine;
    public Collection<HeaderLine> headerLines;

    public static RequestHeader get (String uri, String version)
    {
        final RequestHeader x = new RequestHeader();
        x.requestLine = RequestLine.get(uri, version);
        x.headerLines = new ArrayList<>();
        return x;
    }
}
