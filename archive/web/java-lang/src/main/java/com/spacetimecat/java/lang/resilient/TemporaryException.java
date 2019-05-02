package com.spacetimecat.java.lang.resilient;

final class TemporaryException extends RuntimeException
{
    TemporaryException ()
    {
        super();
    }

    TemporaryException (String message)
    {
        super(message);
    }

    TemporaryException (String message, Throwable cause)
    {
        super(message, cause);
    }

    TemporaryException (Throwable cause)
    {
        super(cause);
    }

    TemporaryException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
