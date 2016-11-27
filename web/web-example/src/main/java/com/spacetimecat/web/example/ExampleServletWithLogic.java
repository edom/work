package com.spacetimecat.web.example;

import com.spacetimecat.web.servlet.HttpServlet2;
import com.spacetimecat.web.servlet.logic.*;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

final class ExampleServletWithLogic extends HttpServlet2
{
    private static final Logic logic =
        First.of(
            new IfMethod("GET", new IfPathInfo("/1", Static.plain("ONE")))
            , new IfMethod("GET", new IfPathInfo("/2", Static.plain("TWO")))
            , new IfMethod("GET",
                new IfPathInfo("/3",
                    First.of(
                        Static.html("<strong>THREE</strong>")
                        , Static.plain("THREE")
                        , new NotAcceptable()
                    )
                )
            )
            , new NotFound()
        );

    @Override
    protected void serviceHttp (HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
        logic.handle(request, response);
    }
}
