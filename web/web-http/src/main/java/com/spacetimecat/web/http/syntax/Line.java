package com.spacetimecat.web.http.syntax;

public final class Line
{
    private final String line;

    public Line (String line)
    {
        this.line = line;
    }

    public RequestLine asRequestLine ()
    {
        return RequestLine.parse(line);
    }

    public StatusLine asStatusLine ()
    {
        return StatusLine.parse(line);
    }

    public HeaderLine asHeaderLine ()
    {
        return HeaderLine.parse(line);
    }

    public boolean isSeparator ()
    {
        return line.equals("\r\n");
    }

    @Override
    public String toString ()
    {
        return line;
    }
}
