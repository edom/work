package com.spacetimecat.collection;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;

/**
 * <p>{@link Iterator} whose elements are {@link Callable}s,
 * supporting parallel execution.</p>
 *
 * <p>The callable must not produce null.</p>
 *
 * @param <A> the type parameter of the {@link Callable}.
 * This cannot be {@link Void}; use {@link com.spacetimecat.Null} instead.
 */
public interface CallableIterator<A> extends Iterator<Callable<A>>
{
    /**
     * <p>Submit all callables for possibly asynchronous execution.</p>
     *
     * <p>The callables are scheduled immediately.
     * The {@link ExecutorService} may run them on other threads.</p>
     *
     * <p>The {@link ExecutorService} must always accept the callables submitted to it.
     * It can block the caller, but it must not throw a
     * {@link java.util.concurrent.RejectedExecutionException}.</p>
     *
     * <p>Exceptions thrown by {@link Future#get()} are wrapped
     * into {@link com.spacetimecat.UncheckedException}.</p>
     *
     * @param es scheduler, thread pool, queue, whatever;
     *
     * @return a lazy view of the results;
     * an {@link Iterator} whose {@link Iterator#next() next} calls {@link Future#get()}
     *
     * @see com.spacetimecat.UncheckedException
     * @see java.util.concurrent.Executors
     */
    Iterator<A> startWith (ExecutorService es);
}
