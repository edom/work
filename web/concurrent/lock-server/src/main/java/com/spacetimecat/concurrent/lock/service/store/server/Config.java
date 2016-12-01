package com.spacetimecat.concurrent.lock.service.store.server;

import java.util.Properties;

final class Config
{
    private final Properties system;

    Config (Properties system)
    {
        this.system = system;
    }

    int getPort ()
    {
        final String string = system.getProperty("port");
        if (string == null)
        {
            throw new IllegalStateException("Pass -Dport=<PORT> to java to specify the port to listen on");
        }
        return Integer.parseInt(string);
    }
}
