package com.spacetimecat.concurrent.batch.internal.call;

import java.util.List;
import java.util.function.Function;

/**
 * <p>
 *     A {@link Function}{@code <List<A>, List<B>>}
 *     with the constraint that the input and output list
 *     must have the same length.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
public interface ListFunction<A, B> extends Function<List<A>, List<B>>
{
    /**
     * <p>
     *     The input list and the output list must have the same size.
     * </p>
     *
     * <p>
     *     The output at position x corresponds to the result of the underlying function
     *     for the input at the same position x.
     * </p>
     *
     * @param inputs
     * list of inputs
     *
     * @return
     * list of outputs
     */
    @Override
    List<B> apply (List<A> inputs);
}
