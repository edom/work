package com.spacetimecat.concurrent.lock;

/**
 * <p>
 *     The caller misuses a {@link Lock} by not properly
 *     pairing {@link Lock#acquire() acquire} and {@link Lock#release() release}.
 * </p>
 *
 * <p>
 *     This should only be used for locks in the same JVM,
 *     but not all such locks should detect this programming error.
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
