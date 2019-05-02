package com.spacetimecat.web.example;

import com.spacetimecat.web.server.DemoServer;

public final class Main
{
    private Main () {}

    public static void main (String[] args) throws InterruptedException
    {
        final int port = 8080;
        final String baseUri = String.format("http://localhost:%s/", port);
        DemoServer.run(port,  new ExampleServlet(baseUri));
    }
}
