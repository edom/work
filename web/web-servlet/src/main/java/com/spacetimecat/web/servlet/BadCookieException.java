package com.spacetimecat.web.servlet;

public class BadCookieException extends BadRequestException
{
    public BadCookieException ()
    {
        super();
    }

    public BadCookieException (String message)
    {
        super(message);
    }

    public BadCookieException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public BadCookieException (Throwable cause)
    {
        super(cause);
    }

    protected BadCookieException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
