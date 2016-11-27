package com.spacetimecat.web.http.syntax;

public final class StatusLine
{
    public boolean valid;
    public String version;
    public int code;
    public String reason;

    public static StatusLine ok (String version)
    {
        final StatusLine x = new StatusLine();
        x.version = version;
        x.code = 200;
        x.reason = "OK";
        return x;
    }

    public static StatusLine noContent (String version)
    {
        final StatusLine x = new StatusLine();
        x.version = version;
        x.code = 204;
        x.reason = "No Content";
        return x;
    }

    public static StatusLine badRequest (String version)
    {
        final StatusLine x = new StatusLine();
        x.version = version;
        x.code = 400;
        x.reason = "Bad Request";
        return x;
    }

    public static StatusLine parse (String line)
    {
        final StatusLine x = new StatusLine();
        final String[] parts = line.split("\\s+", 3);
        if (parts.length >= 3)
        {
            x.valid = true;
            x.version = parts[0];
            x.code = x.readIntOr(parts[1], 0);
            x.reason = parts[2];
        }
        return x;
    }

    private int readIntOr (String str, int def)
    {
        try
        {
            return Integer.parseInt(str);
        }
        catch (NumberFormatException e)
        {
            valid = false;
            return def;
        }
    }
}
