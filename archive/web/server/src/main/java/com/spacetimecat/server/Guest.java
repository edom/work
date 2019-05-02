package com.spacetimecat.server;

import java.io.InputStream;
import java.io.OutputStream;

/**
 * <p>
 *     Source of requests.
 * </p>
 */
public interface Guest extends AutoCloseable
{
    @Override
    void close ();

    /**
     * <p>
     *     The name should be short because it will be used for logging.
     * </p>
     * <p>
     *     For example, this can be the client's IP address.
     * </p>
     * @return name for logging purposes
     */
    String getName ();

    InputStream getInputStream ();

    OutputStream getOutputStream ();
}
