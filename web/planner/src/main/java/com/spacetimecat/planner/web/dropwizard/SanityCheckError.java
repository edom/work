package com.spacetimecat.planner.web.dropwizard;

final class SanityCheckError extends Error
{
    public SanityCheckError ()
    {
        super();
    }

    public SanityCheckError (String message)
    {
        super(message);
    }

    public SanityCheckError (String message, Throwable cause)
    {
        super(message, cause);
    }

    public SanityCheckError (Throwable cause)
    {
        super(cause);
    }

    protected SanityCheckError (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
