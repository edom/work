package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.client.LockClient;

public class ClientMain
{
    public static void main (String[] args) throws InterruptedException
    {
        try (LockClient client = new LockClient("//localhost:8100"))
        {
            final Lock foobar = client.get("foobar");
            for (int i = 0; i < 100; ++i)
            {
                new SlowClient(foobar).run();
                Thread.sleep(1000L);
            }
            new AggressiveClient(client).run();
        }
    }
}
