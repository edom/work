package com.spacetimecat.java.lang.callback;

import java.util.function.Function;

/**
 * <p>
 *     Converts a {@link CallbackFunction} into a {@link Function}.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
public final class Decallbackified<A, B> implements Function<A, B>
{
    private final CallbackFunction<A, B> real;

    public Decallbackified (CallbackFunction<A, B> real)
    {
        this.real = real;
    }

    @Override
    public B apply (A input)
    {
        final Slot<B> blocking = new Slot<>();
        real.apply(input, blocking);
        return blocking.get();
    }
}
