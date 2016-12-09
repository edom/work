package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.service.store.server.ServerMain;

public class ExampleServerMain
{
    public static void main (String[] args)
    {
        try (ServerMain server = new ServerMain(8100))
        {
            server.run();
        }
    }
}
