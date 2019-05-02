package com.spacetimecat.concurrent.lock.service.store.internal.network.io;

public class EofException extends PacketException
{
    public EofException ()
    {
        super();
    }

    public EofException (String message)
    {
        super(message);
    }

    public EofException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public EofException (Throwable cause)
    {
        super(cause);
    }

    protected EofException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
