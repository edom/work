package com.spacetimecat.java.lang.consumer;

import java.util.List;
import java.util.function.Consumer;

public final class ListConsumer<A> implements Consumer<List<A>>
{
    private final List<? extends Consumer<A>> delegates;

    /**
     * <p>
     *     Each delegate must not throw anything.
     * </p>
     *
     * @param delegates
     * list of consumers in this aggregate consumer
     */
    public ListConsumer (List<? extends Consumer<A>> delegates)
    {
        this.delegates = delegates;
    }

    @Override
    public void accept (List<A> values)
    {
        if (delegates.size() != values.size())
        {
            throw new IllegalArgumentException("delegates.size() != values.size()");
        }
        for (int i = 0; i < delegates.size(); ++i)
        {
            final Consumer<A> consumer = delegates.get(i);
            final A value = values.get(i);
            consumer.accept(value);
        }
    }
}
