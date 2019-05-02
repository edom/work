package com.spacetimecat.java.lang.consumer;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;

public final class GatheringConsumer<A>
{
    private final Object lock = new Object();

    private final A[] array;
    private final int count;
    private final Consumer<List<A>> bulkConsumer;

    private int completedCallCount;

    /**
     * <p>
     *     Collects {@code count} things
     *     before passing it to {@code bulkConsumer}.
     * </p>
     *
     * @param count
     * number of things
     *
     * @param bulkConsumer
     * the one expecting a list with that number of things
     */
    @SuppressWarnings("unchecked")
    public GatheringConsumer (int count, Consumer<List<A>> bulkConsumer)
    {
        if (count <= 0) { throw new IllegalArgumentException("count <= 0"); }
        if (bulkConsumer == null) { throw new NullPointerException("bulkConsumer"); }
        this.count = count;
        this.bulkConsumer = bulkConsumer;
        this.array = (A[]) new Object[count];
    }

    /**
     * <p>
     *     You must call the returned consumer's
     *     {@link Consumer#accept(Object) accept}
     *     exactly once: not more, not less.
     * </p>
     *
     * @param index
     * between zero inclusive and {@code count} exclusive
     *
     * @return
     * the consumer for the given index
     */
    public Consumer<A> get (int index)
    {
        if (index < 0) { throw new IllegalArgumentException("index < 0"); }
        if (index >= count) { throw new IllegalArgumentException("index >= count"); }
        return element ->
        {
            boolean thisIsTheLast;
            synchronized (lock)
            {
                array[index] = element;
                ++completedCallCount;
                thisIsTheLast = completedCallCount == count;
            }
            if (thisIsTheLast)
            {
                bulkConsumer.accept(Arrays.asList(array));
            }
        };
    }
}
