package com.spacetimecat.web.servlet;

import com.spacetimecat.web.http.negotiation.Negotiation;
import com.spacetimecat.web.http.param.Parameters;
import com.spacetimecat.web.http.param.Param;

import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public final class Request
{
    private final HttpServletRequest request;

    public Request (HttpServletRequest request)
    {
        this.request = request;
    }

    public Parameters parameters ()
    {
        return new ServletParameters(request);
    }

    public Negotiation negotiation ()
    {
        return Negotiation.withAcceptHeader(request.getHeader("Accept"));
    }

    public String getMethod ()
    {
        return request.getMethod();
    }

    public String getPathInfo ()
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
