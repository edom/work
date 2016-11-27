package com.spacetimecat.web.io;

public final class UnhandledException extends RuntimeException
{
    public UnhandledException ()
    {
        super();
    }

    public UnhandledException (String message)
    {
        super(message);
    }

    public UnhandledException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public UnhandledException (Throwable cause)
    {
        super(cause);
    }

    protected UnhandledException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
