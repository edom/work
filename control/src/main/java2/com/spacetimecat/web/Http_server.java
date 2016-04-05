package com.spacetimecat.web;

import com.spacetimecat.control.Log;
import org.eclipse.jetty.http.HttpURI;
import org.eclipse.jetty.server.*;
import org.eclipse.jetty.server.handler.AbstractHandler;
import org.eclipse.jetty.server.handler.ResourceHandler;

import javax.servlet.ServletException;
import javax.servlet.http.Cookie;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * @author erik
 */
public class Http_server
{

    private final Log log;
    private final Server server;

    public Http_server (Log log, int port)
    {
        this.log = log;
        this.server = new Server();

        // Hide version in 'Server' header.
        final HttpConfiguration config = new HttpConfiguration();
        config.setSendServerVersion(false);
        final HttpConnectionFactory factory = new HttpConnectionFactory(config);
        final ServerConnector connector = new ServerConnector(server, factory);
        connector.setPort(port);
        server.setConnectors(new Connector[] { connector });
    }

    public Http_server logic (Logic logic)
    {
        server.setHandler(new Logic_handler(log, logic));
        return this;
    }

    public Http_server run () throws Exception
    {
        server.start();
        server.join();
        return this;
    }

    private static class Logic_handler extends AbstractHandler
    {
        private final Log log;
        private final Logic logic;

        private Logic_handler (Log log, Logic logic)
        {
            this.log = log;
            this.logic = logic;
        }

        @Override
        public void handle (String target, Request baseRequest, HttpServletRequest request, HttpServletResponse response)
        {
            baseRequest.setHandled(true);
            logic.run_(log, target, baseRequest, response);
        }
    }

    public Http_server static_ (final String dir) throws Exception
    {
        final ResourceHandler handler = new ResourceHandler();
        handler.setDirectoriesListed(true);
        handler.setResourceBase(dir);
        server.setHandler(handler);
        return this;
    }

    /**
     * Subclass this.
     *
     * This class is not thread-safe.
     */
    public abstract static class Logic
    {

        private String target;
        private Request request;
        private HttpServletResponse response;

        protected void delegate (Handler handler) throws IOException, ServletException
        {
            handler.handle(target, request, request, response);
        }

        /**
         * Case-sensitive.
         * @return
         */
        public final String method () { return request.getMethod(); }

        /**
         * Case-sensitive.
         * @param m
         * @return
         */
        public final boolean method_is (String m) { return method().equals(m); }

        /**
         *
         * @param name
         * @return null if there is no such cookie
         */
        public final Cookie cookie (String name)
        {
            final Cookie[] cookies = request.getCookies();
            for (final Cookie c : cookies)
            {
                if (c.getName().equals(name))
                {
                    return c;
                }
            }
            return null;
        }

        public final String target ()
        {
            return target;
        }

        public final Logic content_type (String val)
        {
            response.setContentType(val);
            return this;
        }

        public final Logic header (String key, String value)
        {
            response.addHeader(key, value);
            return this;
        }

        public final Logic ok ()
        {
            response.setStatus(HttpServletResponse.SC_OK);
            return this;
        }

        public final Logic not_found ()
        {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return this;
        }

        public final Logic temporary_redirect ()
        {
            response.setStatus(HttpServletResponse.SC_TEMPORARY_REDIRECT);
            return this;
        }

        public final Logic moved_permanently ()
        {
            response.setStatus(HttpServletResponse.SC_MOVED_PERMANENTLY);
            return this;
        }

        public final String parameter (String name)
        {
            return request.getParameter(name);
        }

        public final String parameter (String name, String def)
        {
            final String val = parameter(name);
            return val == null ? def : val;
        }

        public final HttpURI uri ()
        {
            return request.getUri();
        }

        public final Logic write (String s) throws IOException
        {
            response.getWriter().write(s);
            return this;
        }

        public abstract void run () throws Exception;

        public final void run_ (Log log, String target, Request request, HttpServletResponse response)
        {
            try
            {
                this.target = target;
                this.request = request;
                this.response = response;
                run();
            }
            catch (Exception e)
            {
                log.error(e);
            }
        }

    }

}
