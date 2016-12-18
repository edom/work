package com.spacetimecat.java.lang.callback;

import java.util.function.Consumer;

/**
 * <p>
 *     Wrap a {@link CallbackFunction} into an {@link AbstractCallbackFunction}.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
public final class DelegateCallbackFunction<A, B> extends AbstractCallbackFunction<A, B>
{
    private final CallbackFunction<A, B> delegate;

    public DelegateCallbackFunction (CallbackFunction<A, B> delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public void apply (A input, Consumer<B> outputConsumer)
    {
        delegate.apply(input, outputConsumer);
    }
}
