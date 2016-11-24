package com.spacetimecat.web.servlet;

public class NotFoundException extends HttpException
{
    public NotFoundException ()
    {
        super(404);
    }

    public NotFoundException (String message)
    {
        super(404, message);
    }

    public NotFoundException (String message, Throwable cause)
    {
        super(404, message, cause);
    }

    public NotFoundException (Throwable cause)
    {
        super(404, cause);
    }

    protected NotFoundException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(404, message, cause, enableSuppression, writableStackTrace);
    }
}
