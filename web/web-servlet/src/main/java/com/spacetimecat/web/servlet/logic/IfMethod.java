package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public final class IfMethod implements Logic
{
    private final String method;
    private final Logic then;

    public IfMethod (String method, Logic then)
    {
        this.method = method;
        this.then = then;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        return request.getMethod().equals(method) && then.handle(request, response);
    }
}
