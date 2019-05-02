package com.spacetimecat.java.lang.callback;

import java.util.function.Consumer;
import java.util.function.Function;

/**
 * <p>
 *     Converts an ordinary function to callback-style function.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
public final class Callbackified<A, B> implements CallbackFunction<A, B>
{
    private final Function<A, B> real;

    public Callbackified (Function<A, B> real)
    {
        this.real = real;
    }

    @Override
    public void apply (A input, Consumer<B> outputConsumer)
    {
        final B out = real.apply(input);
        outputConsumer.accept(out);
    }
}
