package com.spacetimecat.web.servlet;

public class BadRequestException extends HttpException
{
    public BadRequestException ()
    {
        super(400);
    }

    public BadRequestException (String message)
    {
        super(400, message);
    }

    public BadRequestException (String message, Throwable cause)
    {
        super(400, message, cause);
    }

    public BadRequestException (Throwable cause)
    {
        super(400, cause);
    }

    protected BadRequestException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(400, message, cause, enableSuppression, writableStackTrace);
    }
}
