package com.spacetimecat.java.util.concurrent;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

/**
 * <p>
 *     This adds some {@link Collection}-accepting
 *     {@code submit} methods to an {@link ExecutorService}.
 * </p>
 */
public interface ExecutorService0 extends ExecutorService, AutoCloseable
{
    /**
     * <p>
     *     This is an alias for {@link #shutdown()}.
     * </p>
     *
     */
    @Override
    void close ();

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
    <T> Future<List<T>> submitAllCallable (Collection<? extends Callable<T>> tasks);

    /**
     * <p>
     *     This is a convenience variant of {@link #submitAllCallable(Collection)}.
     * </p>
     * @param tasks see {@link #submitAllCallable(Collection)}
     * @param <T> see {@link #submitAllCallable(Collection)}
     * @return see {@link #submitAllCallable(Collection)}
     */
    <T> Future<List<T>> submitAllCallable (Callable<T>... tasks);

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
    Future<List<Void>> submitAllRunnable (Collection<? extends Runnable> tasks);

    /**
     * <p>
     *     This is a convenience variant of {@link #submitAllRunnable(Collection)}.
     * </p>
     * @param tasks see {@link #submitAllRunnable(Collection)}
     * @return see {@link #submitAllRunnable(Collection)}
     */
    Future<List<Void>> submitAllRunnable (Runnable... tasks);

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
    Future<Void> submit (Runnable task);
}
