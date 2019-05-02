package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public final class IfPathInfo implements Logic
{
    private final String pathInfo;
    private final Logic then;

    public IfPathInfo (String pathInfo, Logic then)
    {
        this.pathInfo = pathInfo;
        this.then = then;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        return request.getPathInfo().equals(pathInfo) && then.handle(request, response);
    }
}
