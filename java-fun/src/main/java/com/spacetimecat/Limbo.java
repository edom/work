package com.spacetimecat;

import com.spacetimecat.collection.BasicCollection;
import com.spacetimecat.collection.Mutables;

/**
 * <p>A collection of {@link AutoCloseable} resources, and a concise way of closing them.</p>
 *
 * <p>This represents resources that are already opened but might not be used,
 * and this ensures that those resources are closed
 * if any next resources fail to open.</p>
 *
 * <p>The order resources are closed
 * is the reverse of the order they were {@link MutableLimbo#register(AutoCloseable) register}ed.</p>
 *
 * <h3>How to use</h3>
 *
 * <p>Use {@link Limbos#open()} to obtain an instance.</p>
 *
 * <p>In a try block, {@link MutableLimbo#register(AutoCloseable) register}
 * each resource as soon as they are acquired.</p>
 *
 * <p>In the catch block, ensure that you call {@link Limbo#closeAndThrow(Throwable)} closeAndThrow.</p>
 *
 * <p>If you need to close the limbo in non-exceptional condition,
 * use {@link #close()}.</p>
 *
 * <p>If you want to pass along the {@link Limbo},
 * use {@link MutableLimbo#immutable()} to make it immutable.</p>
 *
 * <pre>
 *     final MutableLimbo limbo = Limbos.open();
 *     try
 *     {
 *         final A a = limbo.register(...);
 *         final B b = limbo.register(...);
 *         ...
 *         limbo.close(); // if you need to release it now
 *     }
 *     catch (Throwable e)
 *     {
 *         // if (the method return type is void)
 *         limbo.closeAndThrow(e);
 *         // else
 *         return limbo.closeAndThrow(e);
 *     }
 * </pre>
 *
 * <p>This is similar to Guava's {@code Closer}, but this does not work with Java 6.</p>
 */
public interface Limbo extends
    AutoCloseable
    , BasicCollection<AutoCloseable>
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
     * it is wrapped in an {@link com.spacetimecat.UncheckedException};
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
