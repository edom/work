package com.spacetimecat.concurrent.lock.example;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.client.LockClient;
import org.slf4j.LoggerFactory;

public class ClientMain
{
    public static void main (String[] args) throws InterruptedException
    {
        ((Logger) LoggerFactory.getLogger(Logger.ROOT_LOGGER_NAME)).setLevel(Level.INFO);
        try (LockClient client = new LockClient(new MyListener(), "//localhost:8100"))
        {
            final Lock foobar = client.get("foobar");
            for (int i = 0; i < 3; ++i)
            {
                new SlowClient(foobar).run();
                Thread.sleep(500L);
            }
            new AggressiveClient(client).run();
        }
    }
}
