package com.spacetimecat;

public interface CloseableLimbo extends AutoCloseable
{
    /**
     * <p>Close all resources and throw the throwable.</p>
     *
     * <p>This must be used in a {@code catch (Throwable)} block.</p>
     *
     * <p>Throwables caught while closing the resources
     * become the {@link Throwable#getSuppressed() suppressed} exceptions of t.</p>
     *
     * <p>If the throwable is a checked throwable,
     * it is wrapped in an {@link UncheckedException};
     * otherwise it is thrown as is.</p>
     *
     * @param t cannot be null
     * @param <A> any type; no value is actually ever returned
     *
     * @return this always throws an exception
     */
    <A> A closeAndThrow (Throwable t);

    /**
     * <p>Close all resources.</p>
     *
     * <p>Like {@link #closeAndThrow(Throwable)} but outside a catch block.</p>
     */
    @Override
    void close ();
}
