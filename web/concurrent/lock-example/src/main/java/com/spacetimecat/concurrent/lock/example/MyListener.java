package com.spacetimecat.concurrent.lock.example;

import com.spacetimecat.concurrent.lock.client.LockClientListener;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

final class MyListener extends LockClientListener
{
    private static final Logger logger = LoggerFactory.getLogger(MyListener.class);

    @Override
    protected void enteringAcquire (String name)
    {
        logger.info("{}: acquiring", name);
    }

    @Override
    protected void acquireFailed (String name)
    {
        logger.info("{}: acquire: failed", name);
    }

    @Override
    protected void acquireSucceeded (String name)
    {
        logger.info("{}: acquire: succeeded", name);
    }
}
