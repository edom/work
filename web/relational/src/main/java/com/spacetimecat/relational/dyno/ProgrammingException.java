package com.spacetimecat.relational.dyno;

/**
 * <p>
 *     This is thrown when the client violates certain contracts.
 * </p>
 * <p>
 *     Such error is a programming error that should be fixed by the programmer.
 * </p>
 */
public class ProgrammingException extends RuntimeException
{
    public ProgrammingException ()
    {
        super();
    }

    public ProgrammingException (String message)
    {
        super(message);
    }

    public ProgrammingException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public ProgrammingException (Throwable cause)
    {
        super(cause);
    }
}
