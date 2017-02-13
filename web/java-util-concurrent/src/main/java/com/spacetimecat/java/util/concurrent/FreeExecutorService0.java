package com.spacetimecat.java.util.concurrent;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * <p>
 *     If you give this {@link ExecutorService},
 *     this gives you {@link ExecutorService0}.
 * </p>
 */
public final class FreeExecutorService0 implements ExecutorService0
{
    private final ExecutorService real;

    public FreeExecutorService0 (ExecutorService real)
    {
        this.real = real;
    }

    @Override
    public <T> Future<List<T>> submitAllCallable (Collection<? extends Callable<T>> tasks)
    {
        final List<Future<T>> list = tasks.stream()
            .map(this::submit).collect(Collectors.toList());
        return new FutureList<>(list);
    }

    @Override
    public <T> Future<List<T>> submitAllCallable (Callable<T>... tasks)
    {
        return submitAllCallable(Arrays.asList(tasks));
    }

    @Override
    public Future<List<Void>> submitAllRunnable (Collection<? extends Runnable> tasks)
    {
        final List<Future<Void>> list = tasks.stream()
            .map(this::submit).collect(Collectors.toList());
        return new FutureList<>(list);
    }

    @Override
    public Future<List<Void>> submitAllRunnable (Runnable... tasks)
    {
        return submitAllRunnable(Arrays.asList(tasks));
    }

    // Generated delegates.

    @Override
    public void shutdown ()
    {
        real.shutdown();
    }

    @Override
    public List<Runnable> shutdownNow ()
    {
        return real.shutdownNow();
    }

    @Override
    public boolean isShutdown ()
    {
        return real.isShutdown();
    }

    @Override
    public boolean isTerminated ()
    {
        return real.isTerminated();
    }

    @Override
    public boolean awaitTermination (long timeout, TimeUnit unit) throws InterruptedException
    {
        return real.awaitTermination(timeout, unit);
    }

    @Override
    public <T> Future<T> submit (Callable<T> task)
    {
        return real.submit(task);
    }

    @Override
    public <T> Future<T> submit (Runnable task, T result)
    {
        return real.submit(task, result);
    }

    @Override
    public Future<Void> submit (Runnable task)
    {
        return real.submit(task, null);
    }

    @Override
    public <T> List<Future<T>> invokeAll (Collection<? extends Callable<T>> tasks) throws InterruptedException
    {
        return real.invokeAll(tasks);
    }

    @Override
    public <T> List<Future<T>> invokeAll (Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException
    {
        return real.invokeAll(tasks, timeout, unit);
    }

    @Override
    public <T> T invokeAny (Collection<? extends Callable<T>> tasks) throws InterruptedException, ExecutionException
    {
        return real.invokeAny(tasks);
    }

    @Override
    public <T> T invokeAny (Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException
    {
        return real.invokeAny(tasks, timeout, unit);
    }

    @Override
    public void execute (Runnable command)
    {
        real.execute(command);
    }
}
