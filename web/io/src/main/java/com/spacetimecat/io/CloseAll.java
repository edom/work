package com.spacetimecat.io;

import java.util.Arrays;
import java.util.Collection;

/**
 * <p>
 *     Close all members in spite of throwables.
 * </p>
 * <p>
 *     We can't handle the case where the system runs out of memory
 *     while allocating an instance of this class.
 * </p>
 */
public final class CloseAll implements AutoCloseable
{
    private final Collection<AutoCloseable> collection;

    private CloseAll (Collection<AutoCloseable> collection)
    {
        this.collection = collection;
    }

    public static CloseAll of (Collection<AutoCloseable> items)
    {
        return new CloseAll(items);
    }

    public static CloseAll of (AutoCloseable... items)
    {
        return new CloseAll(Arrays.asList(items));
    }

    /**
     * <p>
     *     Close all members, catching throwables along the way.
     * </p>
     * <p>
     *     The result throwable depends on whether the first throwable is checked.
     *     If the first throwable is checked,
     *     the result throwable is an {@link UnhandledException}
     *     with the checked throwable as the cause
     *     so that the result throwable is unchecked.
     *     The remaining throwables are added as the
     *     {@linkplain Throwable#getSuppressed() suppressed}
     *     throwables of the first throwable (not the result throwable).
     * </p>
     * @throws RuntimeException
     * if the first throwable is a {@link RuntimeException}
     * @throws Error
     * if the first throwable is an {@link Error}
     * @throws UnhandledException
     * if the first throwable would be a checked {@link Throwable}
     */
    @SuppressWarnings("ThrowableResultOfMethodCallIgnored")
    @Override
    public void close ()
    {
        Throwable first = null;
        for (final AutoCloseable item : collection)
        {
            try
            {
                item.close();
            }
            catch (Throwable e)
            {
                if (first == null) { first = e; }
                else { first.addSuppressed(e); }
            }
        }
        if (first == null) { return; }
        if (first instanceof RuntimeException) { throw (RuntimeException) first; }
        if (first instanceof Error) { throw (Error) first; }
        throw new UnhandledException(first);
    }
}
