package com.spacetimecat.java.lang;

/**
 * <p>
 *     Conditional rollback support, for use with try-with-resources.
 * </p>
 */
public final class Rollback implements AutoCloseable
{
    private final Runnable cleanup;

    private boolean ok;

    /**
     * <p>
     *     Initially enabled.
     * </p>
     * @param cleanup
     * to run if this is enabled
     */
    public Rollback (Runnable cleanup)
    {
        this.cleanup = cleanup;
    }

    /**
     * <p>
     *     Set enabled to false so that cleanup will not be run.
     * </p>
     */
    public void disable ()
    {
        ok = true;
    }

    /**
     * <p>
     *     Run the cleanup if this is enabled.
     * </p>
     */
    @Override
    public void close ()
    {
        if (!ok) { cleanup.run(); }
    }
}
