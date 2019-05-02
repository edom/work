package com.spacetimecat.collection;

import com.spacetimecat.function.BasicProcedure1;

/**
 * <p>Provide {@link #forEach(BasicProcedure1)}.</p>
 *
 * <p>Note that you cannot {@code break} or {@code continue} out of the method body
 * because syntactically it is a method body (not loop body).</p>
 *
 * @param <A> element type
 */
public interface ForEach<A>
{
    /**
     * <p>Call the function for each element in this.</p>
     *
     * @param f procedure taking an element
     *
     * @return this
     */
    ForEach<A> forEach (BasicProcedure1<? super A> f);
}
