package com.spacetimecat.web.http.io;

public class EndOfInputException extends RuntimeException
{
    public EndOfInputException ()
    {
        super();
    }

    public EndOfInputException (String message)
    {
        super(message);
    }

    public EndOfInputException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public EndOfInputException (Throwable cause)
    {
        super(cause);
    }

    protected EndOfInputException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
