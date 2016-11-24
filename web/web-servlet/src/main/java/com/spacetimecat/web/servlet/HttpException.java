package com.spacetimecat.web.servlet;

public class HttpException extends RuntimeException
{
    private final int status;

    public HttpException (int status)
    {
        super();
        this.status = status;
    }

    public HttpException (int status, String message)
    {
        super(message);
        this.status = status;
    }

    public HttpException (int status, String message, Throwable cause)
    {
        super(message, cause);
        this.status = status;
    }

    public HttpException (int status, Throwable cause)
    {
        super(cause);
        this.status = status;
    }

    protected HttpException (int status, String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
        this.status = status;
    }

    public final int getStatus ()
    {
        return status;
    }
}
