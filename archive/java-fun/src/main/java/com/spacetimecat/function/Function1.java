package com.spacetimecat.function;

public interface Function1<A, B> extends BasicFunction1<A, B>
{
    /**
     * <p>{@code f.after(g) = g.then(f)}</p>
     * @param that the function to apply first
     * @param <C> input type
     * @return {@code x -> this(that(x))}
     */
    <C> Function1<C, B> after (BasicFunction1<C, A> that);

    /**
     * <p>Compose this and that into {@code x -> that(this(x))}.</p>
     * @param that the function to apply second
     * @param <C> return type
     * @return {@code x -> that(this(x))}
     */
    <C> Function1<A, C> then (BasicFunction1<B, C> that);
}
