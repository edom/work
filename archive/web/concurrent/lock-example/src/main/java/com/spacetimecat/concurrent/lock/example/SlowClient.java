package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.Lock;
import com.spacetimecat.concurrent.lock.WithLock;

final class SlowClient implements Runnable
{
    private final Lock resource;

    SlowClient (Lock resource)
    {
        this.resource = resource;
    }

    @Override
    public void run ()
    {
        new WithLock(resource).run(this::criticalSection);
    }

    private void criticalSection ()
    {
        try
        {
            Thread.sleep(1000L);
        }
        catch (InterruptedException e)
        {
            e.printStackTrace();
        }
    }
}
