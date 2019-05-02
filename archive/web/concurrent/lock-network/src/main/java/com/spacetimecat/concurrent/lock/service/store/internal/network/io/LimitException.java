package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

public class LimitException extends PacketException
{
    public LimitException ()
    {
        super();
    }

    public LimitException (String message)
    {
        super(message);
    }

    public LimitException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public LimitException (Throwable cause)
    {
        super(cause);
    }

    protected LimitException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
