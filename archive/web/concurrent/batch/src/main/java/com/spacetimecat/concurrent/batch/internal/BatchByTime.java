package com.spacetimecat.concurrent.batch.internal;

import com.spacetimecat.concurrent.batch.duration.Duration;
import com.spacetimecat.concurrent.batch.internal.list.BatchingList;
import com.spacetimecat.concurrent.batch.internal.list.BoundedBatchingList;

import java.util.List;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * <p>
 *     Groups things {@link #add(Object) add}ed at about the same time.
 * </p>
 *
 * <p>
 *     For more information, see the
 *     {@linkplain #BatchByTime(ScheduledExecutorService, Duration, BatchingList, Consumer) constructor}.
 * </p>
 *
 * @param <A>
 * thing type
 */
public final class BatchByTime<A> implements AutoCloseable
{
    final Object lock = new Object();

    // JMX counters.
    long counterStartEpochMilli;
    int acceptCount;
    int rejectCount;

    long delay;

    final BatchingList<A> batch;
    private final Consumer<List<A>> consumer;
    private final ScheduledFuture<?> handle;

    /**
     * <p>
     *     Groups things {@link #add(Object) add}ed at about the same time.
     * </p>
     *
     * <p>
     *     The {@code list} parameter can be an instance of {@link BoundedBatchingList}
     *     to prevent the queue from becoming too long.
     * </p>
     *
     * <p>
     *     The {@code delay} parameter is usually an instance of
     *     {@link com.spacetimecat.concurrent.batch.duration.Millisecond Millisecond}.
     *     Lower {@code delay} means lower response time,
     *     but incurs more round-trips and higher thread scheduling overhead.
     * </p>
     *
     * <p>
     *     The {@code consumer} will never be called with an empty list.
     *     If the batch is empty, the consumer is not called at all.
     * </p>
     *
     * <p>
     *     To limit the throughput of this object,
     *     adjust the {@code delay}
     *     and the bounded {@code list} capacity.
     *     For example, to limit the throughput of this object
     *     to 1,000 things every 500 milliseconds,
     *     set {@code delay} to {@code new Millisecond(500)}
     *     and set {@code list} to {@code new BoundedBatchingList(1000)}.
     * </p>
     *
     * <h3>Preconditions</h3>
     *
     * <p>
     *     The consumer must return quickly;
     *     the next batch won't run before the consumer returns.
     * </p>
     *
     * @param executor
     * schedules something to happen periodically
     *
     * @param delay
     * the maximum delay between
     * when a thing is {@link #add(Object) add}ed
     * and when it reaches the {@code consumer}
     *
     * @param list
     * stores batch members before they are processed;
     * should be used only by this object
     *
     * @param consumer
     * what to do with non-empty batch;
     * must return quickly
     */
    public BatchByTime
    (
        ScheduledExecutorService executor
        , Duration delay
        , BatchingList<A> list
        , Consumer<List<A>> consumer
    )
    {
        if (executor == null) { throw new NullPointerException("executor"); }
        if (delay == null) { throw new NullPointerException("delay"); }
        if (list == null) { throw new NullPointerException("list"); }
        if (consumer == null) { throw new NullPointerException("consumer"); }

        this.delay = delay.inMillisecond();
        this.batch = list;
        this.consumer = consumer;

        this.counterStartEpochMilli = System.currentTimeMillis();
        this.handle = executor.scheduleAtFixedRate(this::runBatch, this.delay, this.delay, TimeUnit.MILLISECONDS);
    }

    /**
     * <p>
     *     Add the thing to the current batch.
     * </p>
     *
     * <p>
     *     A return value of false means that this object
     *     is in a temporary over-capacity condition.
     *     You can use this to implement back-pressure.
     * </p>
     *
     * @param thing
     * will be in the current batch
     *
     * @return
     * whether the thing was added to the batch
     */
    public boolean add (A thing)
    {
        synchronized (lock)
        {
            final boolean accepted = batch.add(thing);

            if (accepted)
            {
                if (acceptCount < Integer.MAX_VALUE) { ++acceptCount; }
            }
            else
            {
                if (rejectCount < Integer.MAX_VALUE) { ++rejectCount; }
            }

            return accepted;
        }
    }

    /**
     * <p>
     *     This must not throw anything.
     *     If this does, the scheduled task will stop,
     *     and this will never be called again.
     * </p>
     */
    private void runBatch ()
    {
        final List<A> batch = nextBatch();
        if (!batch.isEmpty())
        {
            consumer.accept(batch);
        }
    }

    private List<A> nextBatch ()
    {
        synchronized (lock)
        {
            return batch.reap();
        }
    }

    /**
     * <p>
     *     Tells the internal thread to stop.
     * </p>
     *
     * <p>
     *     It is a programming error to call {@link #add(Object) add}
     *     after this has been called.
     * </p>
     */
    @Override
    public void close ()
    {
        handle.cancel(false);
    }
}
