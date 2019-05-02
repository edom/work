package com.spacetimecat.java.lang.resilient;

import com.spacetimecat.java.lang.unit.Unit;
import com.spacetimecat.java.lang.unexceptional.Risky;

/**
 * <p>
 *     Where new instances gets born from,
 *     where old instances go to die.
 * </p>
 *
 * <h2>Other names we considered</h2>
 *
 * <p>
 *     {@code Factory} is usually not concerned with the retirement the instances it creates.
 * </p>
 *
 * <p>
 *     {@code Lifecycle} is too abstract.
 * </p>
 *
 * @param <T> instance type
 */
public interface Heaven<T>
{
    /**
     * <p>
     *     Create a new instance.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @return
     * a new instance or a throwable
     */
    Risky<T> create ();

    /**
     * <p>
     *     Close the instance,
     *     release the resources associated with the instance,
     *     ensure that the instance is eligible for garbage collection.
     * </p>
     *
     * <p>
     *     Must not throw anything.
     * </p>
     *
     * @param instance
     * the instance to retire
     *
     * @return
     * whether the instance was successfully retired
     */
    Risky<Unit> retire (T instance);
}
