package com.spacetimecat.java.util.concurrent;

import java.util.Arrays;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * <p>
 *     This adds some {@link Collection}-accepting
 *     {@code submit} methods to an {@link ExecutorService}.
 * </p>
 */
public final class ExecutorService0 implements ExecutorService
{
    private final ExecutorService real;

    public ExecutorService0 (ExecutorService real)
    {
        this.real = real;
    }

    /**
     * <p>
     *     This calls {@link #submit(Callable)} for each task.
     * </p>
     *
     * @param tasks
     * a list of functions
     *
     * @param <T>
     * individual task result type
     *
     * @return
     * a future that will return a list whose order of elements
     * corresponds to the order of the tasks.
     * The first element is the result of the first task,
     * the second is of the second, and so on.
     */
    public <T> Future<List<T>> submitAllCallable (Collection<? extends Callable<T>> tasks)
    {
        final List<Future<T>> list = tasks.stream()
            .map(this::submit).collect(Collectors.toList());
        return new FutureList<>(list);
    }

    /**
     * <p>
     *     This is a convenience variant of {@link #submitAllCallable(Collection)}.
     * </p>
     * @param tasks see {@link #submitAllCallable(Collection)}
     * @param <T> see {@link #submitAllCallable(Collection)}
     * @return see {@link #submitAllCallable(Collection)}
     */
    public <T> Future<List<T>> submitAllCallable (Callable<T>... tasks)
    {
        return submitAllCallable(Arrays.asList(tasks));
    }

    /**
     * <p>
     *     This calls {@link #submit(Runnable)} for each task.
     * </p>
     *
     * @param tasks
     * a list of functions
     *
     * @return
     * a future that will return a list whose order of elements
     * corresponds to the order of the tasks.
     * The first element is the result of the first task,
     * the second is of the second, and so on.
     */
    public Future<List<Void>> submitAllRunnable (Collection<? extends Runnable> tasks)
    {
        final List<Future<Void>> list = tasks.stream()
            .map(this::submit).collect(Collectors.toList());
        return new FutureList<>(list);
    }

    /**
     * <p>
     *     This is a convenience variant of {@link #submitAllRunnable(Collection)}.
     * </p>
     * @param tasks see {@link #submitAllRunnable(Collection)}
     * @return see {@link #submitAllRunnable(Collection)}
     */
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

    /**
     * <p>
     *     This returns {@code Future<Void>},
     *     differing from {@link ExecutorService#submit(Runnable)}
     *     that returns {@code Future<?>}.
     * </p>
     *
     * @param task
     * a function
     *
     * @return
     * see {@link Future} and its methods
     */
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
