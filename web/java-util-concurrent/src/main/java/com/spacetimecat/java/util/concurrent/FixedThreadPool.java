package com.spacetimecat.java.util.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

/**
 * <p>
 *     This improves the usability of {@link ThreadPoolExecutor} for common use cases.
 * </p>
 */
public final class FixedThreadPool implements ExecutorService0
{
    private final ExecutorService0 inner;

    /**
     * <p>
     *     This creates a thread pool implementing {@link ExecutorService0} with a fixed number of threads.
     * </p>
     *
     * <p>
     *     To estimate how many idle threads a machine can handle in 2017,
     *     you can assume that every thread takes 1 MB.
     * </p>
     *
     * @param name
     * is the name of this thread pool.
     * This will be the prefix of the names of the threads in this pool.
     *
     * @param threadCount
     * is the number of threads.
     *
     * @param queueCapacity
     * is the maximum number of waiting tasks.
     * If the queue is full, adding task to this pool
     * will throw a {@link RejectedExecutionException} instead.
     */
    public FixedThreadPool (String name, int threadCount, int queueCapacity)
    {
        final ThreadFactory factory = new MyThreadFactory(name);
        final ThreadPoolExecutor inner = new ThreadPoolExecutor(
            threadCount
            , threadCount
            , 0
            , TimeUnit.MILLISECONDS
            , new ArrayBlockingQueue<>(queueCapacity)
            , factory
        );
        inner.allowCoreThreadTimeOut(false);
        inner.prestartAllCoreThreads();
        this.inner = new FreeExecutorService0(inner);
    }

    // Generated delegates.

    @Override
    public <T> Future<List<T>> submitAllCallable (Collection<? extends Callable<T>> tasks)
    {
        return inner.submitAllCallable(tasks);
    }

    @Override
    public <T> Future<List<T>> submitAllCallable (Callable<T>... tasks)
    {
        return inner.submitAllCallable(tasks);
    }

    @Override
    public Future<List<Void>> submitAllRunnable (Collection<? extends Runnable> tasks)
    {
        return inner.submitAllRunnable(tasks);
    }

    @Override
    public Future<List<Void>> submitAllRunnable (Runnable... tasks)
    {
        return inner.submitAllRunnable(tasks);
    }

    @Override
    public Future<Void> submit (Runnable task)
    {
        return inner.submit(task);
    }

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
