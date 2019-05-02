package com.spacetimecat.concurrent.batch.internal.call;

import com.spacetimecat.java.lang.callback.CallbackFunction;
import com.spacetimecat.java.lang.consumer.FanOutConsumer;

import java.util.List;
import java.util.Objects;
import java.util.function.Consumer;
import java.util.function.Supplier;

/**
 * <p>
 *     Everything related to a method call except the method itself.
 * </p>
 *
 * <p>
 *     This represents an input to a method and
 *     what to do with the return value of the method
 *     without knowing what the method is.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */

public final class Call<A, B> implements Supplier<A>, Consumer<B>
{
    private final A input;
    private final Consumer<B> consumer;

    /**
     * @param input
     * input argument
     *
     * @param consumer
     * what to do with the output (the return value)
     */
    public Call (A input, Consumer<B> consumer)
    {
        this.input = input;
        this.consumer = consumer;
    }

    public static <A, B> Call<A, B> fromEquivalenceClass (List<Call<A, B>> calls)
    {
        if (calls.isEmpty()) { throw new IllegalArgumentException("calls.isEmpty()"); }
        final A input = calls.get(0).input;
        final Consumer<B> fanOut = new FanOutConsumer<>(calls);
        return new Call<>(input, fanOut);
    }

    /**
     * <p>
     *     Finish the call.
     * </p>
     *
     * @param output
     * the return value
     */
    @Override
    public void accept (B output)
    {
        consumer.accept(output);
    }

    /**
     * <p>
     *     Get the input.
     * </p>
     *
     * @return
     * the input
     */
    @Override
    public A get ()
    {
        return input;
    }

    public void runOn (CallbackFunction<A, B> function)
    {
        function.apply(input, consumer);
    }

    @Override
    public int hashCode ()
    {
        return Objects.hashCode(input);
    }

    @Override
    public boolean equals (Object that)
    {
        if (this == that) { return true; }
        if (!(that instanceof Call)) { return false; }
        return Objects.equals(this.input, ((Call) that).input);
    }
}
