package com.spacetimecat.web.servlet;

import com.spacetimecat.web.http.param.Param;

import javax.servlet.http.Cookie;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;

/**
 * <p>
 *     Cookie with proper escaping.
 * </p>
 */
public class Cookie2
{
    private final String name;
    private final String value;

    public Cookie2 (String name, String value)
    {
        this.name = name;
        this.value = value;
    }

    /**
     * <p>
     *     This is tolerant because otherwise
     *     the user won't be able to use the website
     *     if a cookie ever goes bad.
     * </p>
     * <p>
     *     If the name can't be read, it's replaced with empty string.
     * </p>
     * <p>
     *     If the value can't be read, it's replaced with empty string.
     * </p>
     * @param cookie encoded cookie, can't be null
     * @return decoded cookie, not null
     */
    public static Cookie2 decode (Cookie cookie)
    {
        String name = cookie.getName();
        if (name == null) { name = ""; }
        String encoded = cookie.getValue();
        if (encoded == null) { encoded = ""; }
        String value = "";
        try
        {
            value = URLDecoder.decode(encoded, "UTF-8");
        }
        catch (IllegalArgumentException ignored)
        {
        }
        catch (UnsupportedEncodingException e)
        {
            throw new AssertionError(e);
        }
        return new Cookie2(name, value);
    }

    public final String name ()
    {
        return name;
    }

    public final String value ()
    {
        return value;
    }

    public final Cookie encode ()
    {
        try
        {
            final String encoded = URLEncoder.encode(value, "UTF-8");
            final Cookie cookie = new Cookie(name, encoded);
            return cookie;
        }
        catch (UnsupportedEncodingException e)
        {
            throw new AssertionError(e);
        }
    }

    public final Param<String> toParam ()
    {
        return new Param<>(name, value);
    }
}
