package com.spacetimecat.concurrent.batch.function.callback;

import com.spacetimecat.concurrent.batch.duration.Duration;
import com.spacetimecat.concurrent.batch.internal.BatchByTime;
import com.spacetimecat.concurrent.batch.internal.call.Call;
import com.spacetimecat.concurrent.batch.internal.call.Calls;
import com.spacetimecat.concurrent.batch.internal.list.BatchingList;
import com.spacetimecat.concurrent.batch.internal.list.BoundedBatchingList;
import com.spacetimecat.java.lang.callback.CallbackFunction;
import com.spacetimecat.java.lang.callback.ListCallbackFunction;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.function.Consumer;

/**
 * <p>
 *     Delays calls, groups them, runs them together.
 * </p>
 *
 * @param <A>
 * argument type
 *
 * @param <B>
 * return value type
 */
public final class TimeBatchedCallbackFunction<A, B> implements AutoCloseable
{
    final BatchByTime<Call<A, B>> batchByTime;

    /**
     * <p>
     *     Delays calls, groups them, runs them together.
     * </p>
     *
     * <h3>Preconditions</h3>
     *
     * <p>
     *     See the Preconditions of
     *     {@link BatchByTime#BatchByTime(ScheduledExecutorService, Duration, BatchingList, Consumer) BatchByTime}.
     * </p>
     *
     * @param executor
     * schedules things to happen periodically
     *
     * @param delay
     * maximum duration a call is delayed
     * @param capacity
     * maximum number of unserviced delayed calls in the queue
     *
     * @param underlying
     * does the real work;
     * must satisfy the contract of {@link ListCallbackFunction}
     */
    public TimeBatchedCallbackFunction
    (
        ScheduledExecutorService executor
        , Duration delay
        , int capacity
        , CallbackFunction<List<A>, List<B>> underlying
    )
    {
        this.batchByTime = new BatchByTime<>(
            executor, delay, new BoundedBatchingList<>(capacity)
            ,
            calls -> new Calls<>(calls).runOn(underlying)
        );
    }

    /**
     * <p>
     *     Tries to add the function call to the current batch.
     * </p>
     *
     * <p>
     *     If this returns false, the batch is full,
     *     and the consumer will not be called.
     *     The caller should try again later or propagate the error.
     * </p>
     *
     * @param input
     * function argument
     *
     * @param consumer
     * what to do with function return value
     *
     * @return
     * whether the call was added to the batch
     */
    public boolean apply (A input, Consumer<B> consumer)
    {
        return batchByTime.add(new Call<>(input, consumer));
    }

    /**
     * <p>
     *     Stop the internal thread.
     * </p>
     */
    @Override
    public void close ()
    {
        batchByTime.close();
    }
}
