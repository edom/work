package com.spacetimecat.objmap;

import com.spacetimecat.UncheckedException;

public class ReadException extends UncheckedException
{
    public ReadException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public ReadException (Throwable cause)
    {
        super(cause);
    }

    public ReadException (String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace)
    {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
