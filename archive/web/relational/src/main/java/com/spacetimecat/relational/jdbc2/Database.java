package com.spacetimecat.relational.jdbc2;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.function.Consumer;

public final class Database
{
    private final DataSource delegate;

    public Database (DataSource delegate)
    {
        this.delegate = delegate;
    }

    public boolean execute (String sql)
    {
        try (Connection connection = delegate.getConnection())
        {
            return new Connection2(connection).execute(sql);
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }

    public void executeQueryC (String sql, Consumer<RowIterator> consumer)
    {
        try (Connection connection = delegate.getConnection())
        {
            new Connection2(connection).executeQueryC(sql, consumer);
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }
}
