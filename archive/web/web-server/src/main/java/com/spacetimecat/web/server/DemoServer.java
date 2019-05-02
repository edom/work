package com.spacetimecat.web.server;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import javax.servlet.Servlet;
import java.util.Objects;

/**
 * <p>
 *     A canned Jetty server that has only one servlet.
 * </p>
 */
public final class DemoServer
{
    private final Server server;

    private DemoServer (Server server)
    {
        this.server = server;
    }

    public static DemoServer start (int port, Servlet servlet)
    {
        Objects.requireNonNull(servlet, "servlet");
        final Server server = new Server(port);
        final ServletHandler handler = new ServletHandler();
        final ServletHolder holder = new ServletHolder(servlet);
        handler.addServletWithMapping(holder, "/*");
        server.setHandler(handler);
        final DemoServer demo = new DemoServer(server);
        demo.start();
        return demo;
    }

    public static void run (int port, Servlet servlet) throws InterruptedException
    {
        final DemoServer demo = start(port, servlet);
        demo.join();
    }

    private void start ()
    {
        try
        {
            server.start();
        }
        catch (RuntimeException e)
        {
            throw e;
        }
        catch (Exception e)
        {
            throw new RuntimeException(e);
        }
    }

    public void join () throws InterruptedException
    {
        server.join();
    }
}
