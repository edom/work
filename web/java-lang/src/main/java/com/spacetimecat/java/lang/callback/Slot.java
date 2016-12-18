package com.spacetimecat.java.lang.callback;

import com.spacetimecat.java.lang.UncheckedException;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * <p>
 *     Makes a consumer wait for a producer.
 * </p>
 *
 * <p>
 *     This converts a {@link Consumer} (callback-style code)
 *     into a {@link Supplier} (promise-style code).
 *     Indeed, given a {@link java.util.concurrent.BlockingQueue},
 *     every callback-style code
 *     can be converted into a promise-style code.
 * </p>
 *
 * <p>
 *     This is a bounded {@link BlockingQueue} of capacity one
 *     where consumers block until the queue is not empty
 *     and producers block until the queue is not full.
 *     The original use case of this class is exactly
 *     one consumer and exactly one producer
 *     where the consumer waits for the producer.
 * </p>
 *
 * <p>
 *     We use {@link ArrayBlockingQueue} instead of
 *     {@link java.util.concurrent.SynchronousQueue}
 *     because the latter will hang
 *     if the producer and the consumer
 *     are running on the same thread.
 * </p>
 *
 * @param <A>
 * thing type
 */
final class Slot<A> implements Consumer<A>, Supplier<A>
{
    private final BlockingQueue<A> queue = new ArrayBlockingQueue<>(1);

    /**
     * <p>
     *     Blocks until {@link #accept(Object) accept} is called,
     *     and returns what was given to {@link #accept(Object) accept}.
     * </p>
     *
     * <p>
     *     If the thread running this method is interrupted,
     *     this throws an exception.
     * </p>
     *
     * @return
     * what was given to {@link #accept(Object) accept}
     */
    @Override
    public A get ()
    {
        try
        {
            return queue.take();
        }
        catch (InterruptedException e)
        {
            Thread.currentThread().interrupt();
            throw new UncheckedException(e);
        }
    }

    /**
     * <p>
     *     Sends something to the consumer,
     *     blocking if the consumer has not yet consumed the previous value.
     * </p>
     *
     * @param value
     * what to send
     */
    @Override
    public void accept (A value)
    {
        try
        {
            queue.put(value);
        }
        catch (InterruptedException ignored)
        {
            Thread.currentThread().interrupt();
        }
    }
}
