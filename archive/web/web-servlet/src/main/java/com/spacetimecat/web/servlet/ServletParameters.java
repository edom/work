package com.spacetimecat.web.servlet;

import com.spacetimecat.web.http.param.Parameters;
import com.spacetimecat.web.http.param.Param;

import javax.servlet.http.HttpServletRequest;

public final class ServletParameters extends Parameters
{
    private final HttpServletRequest request;

    public ServletParameters (HttpServletRequest request)
    {
        this.request = request;
    }

    @Override
    public Param<String> get (String name)
    {
        final String value = request.getParameter(name);
        return new Param<>(name, value);
    }
}
