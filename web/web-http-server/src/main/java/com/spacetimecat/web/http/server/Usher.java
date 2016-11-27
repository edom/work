package com.spacetimecat.web.http.server;

/**
 * <p>
 *     Serve a guest, or lead a guest to something that can serve it.
 * </p>
 */
public interface Usher extends AutoCloseable
{
    @Override
    void close ();

    /**
     * <p>
     *     This must close the guest if there are no seats left.
     * </p>
     * <p>
     *     This must not throw any exception.
     * </p>
     * @param guest
     * a source of requests
     */
    void handle (Guest guest);
}
