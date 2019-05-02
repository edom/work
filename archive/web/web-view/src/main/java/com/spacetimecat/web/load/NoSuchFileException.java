package com.spacetimecat.web.load;

public final class NoSuchFileException extends LoadException
{
    public NoSuchFileException (String message)
    {
        super(message);
    }

    public NoSuchFileException (Throwable cause)
    {
        super(cause);
    }
}
