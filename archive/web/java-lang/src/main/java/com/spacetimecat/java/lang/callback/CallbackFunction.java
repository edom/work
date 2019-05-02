package com.spacetimecat.java.lang.callback;

import java.util.function.Consumer;

/**
 * <p>
 *     A function in callback style.
 * </p>
 *
 * <p>
 *     The reason of writing a function in this style
 *     is that it does not have to 'return' right away.
 *     It does not have to 'return' exactly once.
 *     It does not have to 'return' at all.
 *     A CPS function 'returns' by calling the consumer with its return value.
 *     The {@link #apply(Object, Consumer) apply} function does return to the caller.
 * </p>
 *
 * <p>
 *     The drawback is that it messes your indentation.
 * </p>
 *
 * <p>
 *     Unlike Scheme, Java (version 8 at the time of this writing)
 *     does not have proper tail calls and continuations.
 *     A composed {@code CallbackFunction} will need stack size proportional
 *     to the number of the functions in it when {@link #apply(Object, Consumer) apply}ed.
 * </p>
 *
 * <p>
 *     Do not confuse a callback-function and a callback.
 *     A callback-function has the type {@code a -> Callback b -> ()}
 *     whereas a callback has the type {@code a -> ()}.
 *     In Java, a callback-function is a
 *     {@link java.util.function.BiConsumer BiConsumer}
 *     whose the second argument is a callback,
 *     whereas a callback is a {@link Consumer}.
 * </p>
 *
 * <p>
 *     To convert an instance of this into a {@linkplain java.util.function.Function function},
 *     use {@link Decallbackified}.
 * </p>
 *
 * @param <A>
 * input type
 *
 * @param <B>
 * output type
 */
@FunctionalInterface
public interface CallbackFunction<A, B>
{
    /**
     * <p>
     *     Call the function.
     * </p>
     *
     * @param input
     * function argument
     *
     * @param outputConsumer
     * what to do with the function return value
     */
    void apply (A input, Consumer<B> outputConsumer);
}
