package com.spacetimecat.java.lang.consumer;

import java.util.List;
import java.util.function.Consumer;

public final class FanOutConsumer<A> implements Consumer<A>
{
    private final List<? extends Consumer<A>> consumers;

    public FanOutConsumer (List<? extends Consumer<A>> consumers)
    {
        this.consumers = consumers;
    }

    @Override
    public void accept (A thing)
    {
        consumers.forEach(c -> c.accept(thing));
    }
}
