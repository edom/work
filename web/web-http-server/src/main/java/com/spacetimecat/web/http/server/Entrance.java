package com.spacetimecat.web.http.server;

/**
 * <p>
 *     Queue of {@link Guest}s.
 * </p>
 */
public interface Entrance extends AutoCloseable
{
    @Override
    void close ();

    /**
     * <p>
     *     This should throw an exception if the entrance has been closed.
     * </p>
     * @return
     * the next client
     */
    Guest next ();
}
