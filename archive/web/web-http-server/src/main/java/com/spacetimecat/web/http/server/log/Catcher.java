package com.spacetimecat.web.http.server.log;

/**
 * <p>
 *     Log {@link Throwable}s.
 * </p>
 */
@FunctionalInterface
public interface Catcher
{
    /**
     * <p>
     *     This must not throw anything.
     * </p>
     * @param throwable
     * the thing of interest
     */
    void accept (Throwable throwable);
}
