package com.spacetimecat.web.servlet;

import javax.servlet.GenericServlet;
import javax.servlet.ServletException;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * <p>
 *     Like {@link javax.servlet.http.HttpServlet} but more composable.
 * </p>
 */
public abstract class HttpServlet2 extends GenericServlet
{
    @Override
    public final void service (ServletRequest request, ServletResponse response)
        throws ServletException, IOException
    {
        if (!(request instanceof HttpServletRequest))
        {
            throw new UnsupportedOperationException("!(request instanceof HttpServletRequest)");
        }
        if (!(response instanceof HttpServletResponse))
        {
            throw new UnsupportedOperationException("!(response instanceof HttpServletResponse)");
        }
        serviceHttp((HttpServletRequest) request, (HttpServletResponse) response);
    }

    protected abstract void serviceHttp (HttpServletRequest request, HttpServletResponse response)
        throws ServletException, IOException;
}
