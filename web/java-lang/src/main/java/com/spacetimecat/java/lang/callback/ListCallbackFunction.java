package com.spacetimecat.java.lang.callback;

import java.util.List;
import java.util.function.Consumer;

/**
 * <p>
 *     A {@link CallbackFunction} whose
 *     input list and output list must have the same length.
 * </p>
 *
 * @param <A>
 * input list element type
 *
 * @param <B>
 * output list element type
 */
public interface ListCallbackFunction<A, B> extends CallbackFunction<List<A>, List<B>>
{
    /**
     * <p>
     *     Both the input list and the output list must have the same length.
     * </p>
     *
     * @param inputs
     * input list
     *
     * @param outputConsumer
     * what to do with output list
     */
    @Override
    void apply (List<A> inputs, Consumer<List<B>> outputConsumer);
}
