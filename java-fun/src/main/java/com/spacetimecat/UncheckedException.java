package com.spacetimecat;

/**
 * <p>This wraps a checked {@link Exception} so that it can be thrown without much fuss.</p>
 *
 * <p>An {@link UncheckedException} must have a {@link #getCause() cause}.</p>
 */
public final class UncheckedException extends RuntimeException
{
    public UncheckedException (String message, Throwable cause)
    {
        super(message, cause);
        check(cause);
    }

    public UncheckedException (Throwable cause)
    {
        super(cause);
        check(cause);
    }

    public UncheckedException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
        check(cause);
    }

    private static void check (Throwable cause)
    {
        if (cause == null) { throw new NullPointerException("cause cannot be null"); }
    }
}
