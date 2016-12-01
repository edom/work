package com.spacetimecat.concurrent.semaphore;

public class SemaphoreException extends RuntimeException
{
    public SemaphoreException ()
    {
        super();
    }

    public SemaphoreException (String message)
    {
        super(message);
    }

    public SemaphoreException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public SemaphoreException (Throwable cause)
    {
        super(cause);
    }

    protected SemaphoreException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
