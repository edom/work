package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

public class PacketException extends RuntimeException
{
    public PacketException ()
    {
        super();
    }

    public PacketException (String message)
    {
        super(message);
    }

    public PacketException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public PacketException (Throwable cause)
    {
        super(cause);
    }

    protected PacketException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
