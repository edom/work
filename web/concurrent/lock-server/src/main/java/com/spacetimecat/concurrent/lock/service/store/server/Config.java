package com.spacetimecat.concurrent.lock.service.store.server;

import java.util.Map;

final class Config
{
    private final Map<String, String> system;

    Config (Map<String, String> system)
    {
        this.system = system;
    }

    int getPort ()
    {
        final String string = system.get("port");
        if (string == null)
        {
            throw new IllegalStateException("Set the 'port' environment variable to specify the port to listen on");
        }
        return Integer.parseInt(string);
    }
}
