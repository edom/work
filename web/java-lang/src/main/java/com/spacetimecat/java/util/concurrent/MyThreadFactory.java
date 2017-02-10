package com.spacetimecat.java.util.concurrent;

import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;

final class MyThreadFactory implements ThreadFactory
{
    private final String prefix;
    private final AtomicInteger count;

    MyThreadFactory (String prefix)
    {
        this.prefix = prefix;
        this.count = new AtomicInteger();
    }

    @Override
    public Thread newThread (Runnable r)
    {
        final Thread thread = new Thread(r);
        final int number = count.getAndIncrement();
        thread.setName(prefix + "-" + number);
        return thread;
    }
}
