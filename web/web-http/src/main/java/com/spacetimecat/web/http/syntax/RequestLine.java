package com.spacetimecat.web.http.syntax;

public final class RequestLine
{
    public boolean valid;
    public String method;
    public String uri;
    public String version;
    public String unparsed;

    public static RequestLine parse (String line)
    {
        final RequestLine result = new RequestLine();
        final String[] parts = line.split("\\s+");
        if (parts.length >= 3)
        {
            result.valid = true;
            result.method = parts[0];
            result.uri = parts[1];
            result.version = parts[2];
            result.unparsed = line;
        }
        return result;
    }

    public static RequestLine get (String uri, String version)
    {
        final RequestLine x = new RequestLine();
        x.method = "GET";
        x.uri = uri;
        x.version = version;
        return x;
    }
}
