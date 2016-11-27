package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.Arrays;
import java.util.Collection;

public final class First implements Logic
{
    private final Collection<Logic> logics;

    private First (Collection<Logic> logics)
    {
        this.logics = logics;
    }

    public static Logic of (Collection<Logic> logics)
    {
        return new First(logics);
    }

    public static Logic of (Logic... logics)
    {
        return of(Arrays.asList(logics));
    }

    @Override
    public boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException
    {
        for (Logic logic : logics)
        {
            if (logic.handle(request, response))
            {
                return true;
            }
        }
        return false;
    }
}
