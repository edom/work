package com.spacetimecat.java.lang.resilient;

final class ClosedException extends RuntimeException
{
    ClosedException ()
    {
        super();
    }

    ClosedException (String message)
    {
        super(message);
    }

    ClosedException (String message, Throwable cause)
    {
        super(message, cause);
    }

    ClosedException (Throwable cause)
    {
        super(cause);
    }

    ClosedException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
