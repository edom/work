package com.spacetimecat.web.http.syntax;

import java.util.ArrayList;
import java.util.Collection;

public final class ResponseHeader
{
    public boolean valid;
    public StatusLine statusLine;
    public Collection<HeaderLine> headerLines;

    public static ResponseHeader ok (String version)
    {
        final ResponseHeader result = new ResponseHeader();
        result.statusLine = StatusLine.ok(version);
        result.headerLines = new ArrayList<>();
        return result;
    }

    public static ResponseHeader noContent (String version)
    {
        final ResponseHeader result = new ResponseHeader();
        result.statusLine = StatusLine.noContent(version);
        result.headerLines = new ArrayList<>();
        return result;
    }

    public static ResponseHeader badRequest (String version)
    {
        final ResponseHeader result = new ResponseHeader();
        result.statusLine = StatusLine.badRequest(version);
        result.headerLines = new ArrayList<>();
        return result;
    }
}
