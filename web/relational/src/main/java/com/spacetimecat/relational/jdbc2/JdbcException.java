package com.spacetimecat.relational.jdbc2;

public class JdbcException extends RuntimeException
{
    public JdbcException ()
    {
        super();
    }

    public JdbcException (String message)
    {
        super(message);
    }

    public JdbcException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public JdbcException (Throwable cause)
    {
        super(cause);
    }
}
