package com.spacetimecat.web.servlet;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

public class Request
{
    private final HttpServletRequest request;

    public Request (HttpServletRequest request)
    {
        this.request = request;
    }

    public final Param<String> getParameter (String name)
    {
        final String value = request.getParameter(name);
        return new Param<>(name, value);
    }

    public final String getMethod ()
    {
        return request.getMethod();
    }

    public final String getPathInfo ()
    {
        return request.getPathInfo();
    }

    public Param<String> getCookieValue (String name)
    {
        final List<Cookie> cookies = list(request.getCookies());
        for (final Cookie encodedCookie : cookies)
        {
            final Cookie2 cookie = Cookie2.decode(encodedCookie);
            if (cookie.name().equals(name))
            {
                return cookie.toParam();
            }
        }
        return new Param<>(name, null);
    }

    private static <T> List<T> list (T[] array)
    {
        if (array == null) { return Collections.emptyList(); }
        return Arrays.asList(array);
    }
}
