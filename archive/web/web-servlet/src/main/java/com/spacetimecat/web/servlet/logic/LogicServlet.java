package com.spacetimecat.web.servlet.logic;

import com.spacetimecat.web.servlet.HttpServlet2;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * <p>
 *     An {@link HttpServlet2} that delegates processing to a {@link Logic}.
 * </p>
 */
public final class LogicServlet extends HttpServlet2
{
    private final Logic logic;

    public LogicServlet (Logic logic)
    {
        this.logic = logic;
    }

    /**
     * <p>
     *     If the logic returns false, this sends 404.
     * </p>
     * @param request
     * request
     * @param response
     * response
     */
    @Override
    protected void serviceHttp (HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException
    {
        if (!logic.handle(request, response))
        {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
        }
    }
}
