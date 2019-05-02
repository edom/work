package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public final class If implements Logic
{
    private final Logic condition;
    private final Logic ifTrue;

    public If (Logic condition, Logic ifTrue)
    {
        this.condition = condition;
        this.ifTrue = ifTrue;
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        return condition.handle(request, response) && ifTrue.handle(request, response);
    }
}
