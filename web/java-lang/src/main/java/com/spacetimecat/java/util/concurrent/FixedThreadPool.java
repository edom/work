package com.spacetimecat.java.util.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

/**
 * <p>
 *     This improves the usability of {@link ScheduledThreadPoolExecutor} for common use cases.
 * </p>
 *
 * <p>
 *     To estimate how many idle threads a machine can handle,
 *     you can assume that every thread takes 1 MB.
 * </p>
 */
public final class FixedThreadPool implements ExecutorService
{
    private final ThreadPoolExecutor inner;

    /**
     * <p>
     *     A thread pool with a fixed number of threads.
     * </p>
     *
     * @param name
     * the name of this thread pool;
     * this will be the prefix of the names of the threads in this pool
     *
     * @param threadCount
     * the number of threads
     *
     * @param queueCapacity
     * the number of
     */
    public FixedThreadPool (String name, int threadCount, int queueCapacity)
    {
        final ThreadFactory factory = new MyThreadFactory(name);
        inner = new ThreadPoolExecutor(
            threadCount
            , threadCount
            , 0
            , TimeUnit.MILLISECONDS
            , new ArrayBlockingQueue<>(queueCapacity)
            , factory
        );
        inner.allowCoreThreadTimeOut(false);
        inner.prestartAllCoreThreads();
    }

    // Generated delegates.

    @Override
    public void shutdown ()
    {
        inner.shutdown();
    }

    @Override
    public List<Runnable> shutdownNow ()
    {
        return inner.shutdownNow();
    }

    @Override
    public boolean isShutdown ()
    {
        return inner.isShutdown();
    }

    @Override
    public boolean isTerminated ()
    {
        return inner.isTerminated();
    }

    @Override
    public boolean awaitTermination (long timeout, TimeUnit unit) throws InterruptedException
    {
        return inner.awaitTermination(timeout, unit);
    }

    @Override
    public <T> Future<T> submit (Callable<T> task)
    {
        return inner.submit(task);
    }

    @Override
    public <T> Future<T> submit (Runnable task, T result)
    {
        return inner.submit(task, result);
    }

    @Override
    public Future<?> submit (Runnable task)
    {
        return inner.submit(task);
    }

    @Override
    public <T> List<Future<T>> invokeAll (Collection<? extends Callable<T>> tasks) throws InterruptedException
    {
        return inner.invokeAll(tasks);
    }

    @Override
    public <T> List<Future<T>> invokeAll (Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException
    {
        return inner.invokeAll(tasks, timeout, unit);
    }

    @Override
    public <T> T invokeAny (Collection<? extends Callable<T>> tasks) throws InterruptedException, ExecutionException
    {
        return inner.invokeAny(tasks);
    }

    @Override
    public <T> T invokeAny (Collection<? extends Callable<T>> tasks, long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException
    {
        return inner.invokeAny(tasks, timeout, unit);
    }

    @Override
    public void execute (Runnable command)
    {
        inner.execute(command);
    }
}
