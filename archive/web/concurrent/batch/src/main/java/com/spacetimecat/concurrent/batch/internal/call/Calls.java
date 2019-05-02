package com.spacetimecat.concurrent.batch.internal.call;

import com.spacetimecat.java.lang.callback.CallbackFunction;
import com.spacetimecat.java.lang.consumer.ListConsumer;
import com.spacetimecat.java.lang.supplier.ListSupplier;

import java.util.List;

/**
 * <p>
 *     List of {@link Call}s.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
public final class Calls<A, B>
{
    private final List<Call<A, B>> list;

    public Calls (List<Call<A, B>> list)
    {
        this.list = list;
    }

    private List<A> getInputs ()
    {
        return new ListSupplier<>(list).get();
    }

    private void finishWith (List<B> outputs)
    {
        new ListConsumer<>(list).accept(outputs);
    }

    public void runOn (ListFunction<A, B> function)
    {
        final List<A> inputs = getInputs();
        final List<B> outputs = function.apply(inputs);
        assert inputs.size() == outputs.size();
        finishWith(outputs);
    }

    public void runOn (CallbackFunction<List<A>, List<B>> function)
    {
        final List<A> inputs = getInputs();
        function.apply(inputs, new ListConsumer<>(list));
    }
}
