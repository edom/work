package com.spacetimecat.java.util.concurrent;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

final class FutureList<A> implements Future<List<A>>
{
    private final List<Future<A>> futures;

    FutureList (List<Future<A>> futures)
    {
        this.futures = futures;
    }

    @Override
    public boolean cancel (boolean interrupt)
    {
        return futures.stream()
            .map(x -> x.cancel(interrupt))
            .reduce(false, (x, y) -> x | y);
    }

    @Override
    public boolean isCancelled ()
    {
        return futures.stream().anyMatch(Future::isCancelled);
    }

    @Override
    public boolean isDone ()
    {
        return futures.stream().allMatch(Future::isDone);
    }

    @Override
    public List<A> get () throws InterruptedException, ExecutionException
    {
        final List<A> list = new ArrayList<>(futures.size());
        for (Future<A> future : futures)
        {
            final A thing = future.get();
            list.add(thing);
        }
        return list;
    }

    @Override
    public List<A> get (long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException
    {
        final List<A> list = new ArrayList<>(futures.size());
        final long patience = unit.toNanos(timeout);
        final long deadline = System.nanoTime() + patience;
        for (Future<A> future : futures)
        {
            final long now = System.nanoTime();
            final long remaining = deadline - now;
            final A thing = future.get(remaining, TimeUnit.NANOSECONDS);
            list.add(thing);
        }
        return list;
    }
}
