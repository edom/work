package com.spacetimecat.relational.jdbc2;

public class ColumnNotFoundException extends JdbcException
{
    public ColumnNotFoundException ()
    {
        super();
    }

    public ColumnNotFoundException (String message)
    {
        super(message);
    }

    public ColumnNotFoundException (String message, Throwable cause)
    {
        super(message, cause);
    }

    public ColumnNotFoundException (Throwable cause)
    {
        super(cause);
    }
}
