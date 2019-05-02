package com.spacetimecat.relational.jdbc2;

import java.sql.Connection;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.function.Consumer;
import java.util.function.Function;

/**
 * <p>
 *     Start here: This wraps a {@link Connection} and always sets auto-commit to true.
 * </p>
 */
public final class Connection2 implements AutoCloseable
{
    private final Connection delegate;

    public Connection2 (Connection delegate) throws SQLException
    {
        delegate.setAutoCommit(true);
        this.delegate = delegate;
    }

    /**
     * <p>
     *     Escape hatch.
     * </p>
     * @return the delegate
     */
    public Connection unwrap ()
    {
        return delegate;
    }

    @Override
    public void close ()
    {
        try
        {
            delegate.close();
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }

    public boolean execute (String sql)
    {
        try (Statement statement = delegate.createStatement())
        {
            return statement.execute(sql);
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }

    public <A> A executeQueryF (String sql, Function<RowIterator, A> consumer)
    {
        try
        (
            Statement statement = delegate.createStatement();
            ResultSet resultSet = statement.executeQuery(sql);
        )
        {
            return consumer.apply(new RowIterator(resultSet));
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }

    public void executeQueryC (String sql, Consumer<RowIterator> consumer)
    {
        final Function<RowIterator, Object> function = rows ->
        {
            consumer.accept(rows);
            return null;
        };
        executeQueryF(sql, function);
    }
}
