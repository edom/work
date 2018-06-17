package com.spacetimecat.meta.har.java;

import com.google.inject.Guice;
import com.google.inject.Injector;
import com.spacetimecat.meta.example.MySiteHttpServlet;
import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import javax.servlet.Servlet;

public final class Main {
    public static void main (String[] args) throws Exception {
        final Injector injector = Guice.createInjector(new MyModule());
        // http://www.eclipse.org/jetty/documentation/current/embedding-jetty.html
        final Server server = new Server(8080);
        {
            final ServletHandler handler = new ServletHandler();
            server.setHandler(handler);
            final Servlet servlet = injector.getInstance(MySiteHttpServlet.class);
            final ServletHolder holder = new ServletHolder(servlet);
            handler.addServletWithMapping(holder, "/*");
        }
        server.start();
        server.join();
    }
}
