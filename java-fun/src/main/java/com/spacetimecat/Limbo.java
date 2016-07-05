package com.spacetimecat;

/**
 * <p>Separate resource acquisition and release.</p>
 *
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
 * cast it to a {@link MutableLimbo}.</p>
 *
 * <pre>
 *     final Limbo limbo = Limbos.open();
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
 * <p>You can pass the responsibility of freeing the resources to the caller
 * by adding a {@link MutableLimbo} argument to your method
 * and {@link MutableLimbo#register(AutoCloseable) register}ing
 * every resources that should be closed later.
 * The caller decides when those resources are closed.</p>
 *
 * <p>This is similar to Guava's {@code Closer}, but this does not work with Java 6.</p>
 */
public interface Limbo extends
    CloseableLimbo
    , MutableLimbo
{
    @Override
    Limbo add (AutoCloseable resource);
}
