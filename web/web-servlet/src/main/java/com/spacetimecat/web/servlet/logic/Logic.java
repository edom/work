package com.spacetimecat.web.servlet.logic;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * <p>
 *     Match a request and respond if the request matches.
 * </p>
 */
@FunctionalInterface
public interface Logic
{
    /**
     * <p>
     *     Like {@linkplain com.spacetimecat.web.servlet.HttpServlet2#serviceHttp(HttpServletRequest, HttpServletResponse) serviceHttp}
     *     but returns a boolean.
     * </p>
     * <p>
     *     If this wants to write the response body,
     *     this must use only {@linkplain HttpServletResponse#getOutputStream() getOutputStream}.
     *     It must not use {@linkplain HttpServletResponse#getWriter() getWriter}.
     * </p>
     * <p>
     *     This must not flush the output stream.
     * </p>
     * <p>
     *     If this returns false, this must not modify the response.
     * </p>
     * @param request
     * request; assume that this is immutable
     * @param response
     * mutable response; the method works by mutating this response
     * @return
     * whether this logic matches (handles) the request.
     * If this is true, processing stops and the response will be sent.
     * If this is false, the response must not be modified.
     *
     * @throws IOException
     * input-output error
     *
     * @throws ServletException
     * other error
     */
    boolean handle (HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException;
}
