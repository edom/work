package com.spacetimecat.web.servlet;

import javax.servlet.http.HttpServletResponse;

public class Response
{
    private final HttpServletResponse response;

    public Response (HttpServletResponse response)
    {
        this.response = response;
    }

    public final void addCookie (Cookie2 cookie)
    {
        response.addCookie(cookie.encode());
    }
}
