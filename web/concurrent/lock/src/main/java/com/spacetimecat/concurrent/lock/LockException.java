package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     The caller tries to release a lock that is not engaged.
 * </p>
 *
 * <p>
 *     In the case of local locks,
 *     this means that there is a programming error.
 * </p>
 *
 * <p>
 *     In the case of remote locks,
 *     this may mean that the lock server is restarted too soon.
 * </p>
 *
 * <p>
 *     This should not be used to signal input-output error.
 *     Use another exception such as {@link java.io.UncheckedIOException} for that.
 * </p>
 */
public class LockException extends RuntimeException
{
    public LockException ()
    {
        super();
    }

    public LockException (String message)
    {
        super(message);
    }

    public LockException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public LockException (Throwable cause)
    {
        super(cause);
    }

    protected LockException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
