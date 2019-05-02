package com.spacetimecat;

import com.spacetimecat.function.BasicCheckedFunction1;

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
 * <h2>How to use</h2>
 *
 * <h3>If you do not need special exception handling</h3>
 *
 * <p>Use {@link Limbos#with(BasicCheckedFunction1)}.</p>
 *
 * <pre>
 *     return Limbos.with(limbo -&gt; {
 *         final A a = limbo.register(...);
 *         final B b = limbo.register(...);
 *         return ...;
 *     });
 * </pre>
 *
 * <p>When execution leaves the block (because the code returns normally or throws something),
 * {@link Limbos#with(BasicCheckedFunction1)} will close all resources that had been
 * {@link #register(AutoCloseable) register}ed so far.</p>
 *
 * <h3>If you need special exception handling</h3>
 *
 * <p>Use {@link Limbos#open()} to obtain an instance.</p>
 *
 * <p>In a try block, {@link MutableLimbo#register(AutoCloseable) register}
 * each resource as soon as they are acquired.</p>
 *
 * <p>In the catch block, ensure that you call
 * {@link Limbo#closeAndThrowUnchecked(Throwable)} closeAndThrowUnchecked.</p>
 *
 * <p>If you need to close the limbo in non-exceptional condition,
 * use {@link #close()}.</p>
 *
 * <p>If you want to pass along the {@link Limbo},
 * the callee should cast it to {@link MutableLimbo}
 * to indicate that the callee should not call {@link #close()}.</p>
 *
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
 *         limbo.closeAndThrowUnchecked(e);
 *         // else
 *         return limbo.closeAndThrowUnchecked(e);
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
