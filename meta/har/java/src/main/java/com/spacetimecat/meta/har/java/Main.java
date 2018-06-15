package com.spacetimecat.meta.har.java;

import com.spacetimecat.MySiteHttpServlet;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHandler;

public final class Main {
    public static void main (String[] args) throws Exception {
        // http://www.eclipse.org/jetty/documentation/current/embedding-jetty.html
        final Server server = new Server(8080);
        {
            final ServletHandler handler = new ServletHandler();
            server.setHandler(handler);
            handler.addServletWithMapping(MySiteHttpServlet.class, "/*");
        }
        server.start();
        server.join();
    }
}
