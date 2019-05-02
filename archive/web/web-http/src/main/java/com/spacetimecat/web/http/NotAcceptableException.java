package com.spacetimecat.web.http;

/**
 * <p>
 *     406 not acceptable.
 * </p>
 */
public class NotAcceptableException extends HttpException
{
    public NotAcceptableException ()
    {
        super(406);
    }

    public NotAcceptableException (String message)
    {
        super(406, message);
    }

    public NotAcceptableException (String message, Throwable cause)
    {
        super(406, message, cause);
    }

    public NotAcceptableException (Throwable cause)
    {
        super(406, cause);
    }

    protected NotAcceptableException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(406, message, cause, enableSuppression, writableStackTrace);
    }
}
