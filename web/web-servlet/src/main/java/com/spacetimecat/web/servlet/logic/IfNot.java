package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public final class IfNot implements Logic
{
    private final Logic condition;
    private final Logic ifFalse;

    public IfNot (Logic condition, Logic ifFalse)
    {
        this.condition = condition;
        this.ifFalse = ifFalse;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        return condition.handle(request, response) || ifFalse.handle(request, response);
    }
}
