package com.spacetimecat.java.lang.callback;

public abstract class AbstractCallbackFunction<A, B> implements CallbackFunction<A, B>
{
    /**
     * <p>
     *     Returns a function that feeds the output of this to that;
     *     roughly {@code f.compose(g) = x -> g(f(x))}.
     * </p>
     *
     * @param that
     * takes the output of this
     *
     * @param <C>
     * return type
     *
     * @return
     * see description
     */
    public final <C> CallbackFunction<A, C> then (CallbackFunction<B, C> that)
    {
        return (a, c) -> this.apply(a, b -> that.apply(b, c));
    }
}
