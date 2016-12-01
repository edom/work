package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

public class FormatException extends PacketException
{
    public FormatException ()
    {
        super();
    }

    public FormatException (String message)
    {
        super(message);
    }

    public FormatException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public FormatException (Throwable cause)
    {
        super(cause);
    }

    protected FormatException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
