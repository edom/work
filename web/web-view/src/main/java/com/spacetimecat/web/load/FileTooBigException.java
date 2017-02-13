package com.spacetimecat.web.load;

public final class FileTooBigException extends LoadException
{
    public FileTooBigException (String message)
    {
        super(message);
    }
}
