package com.spacetimecat.relational.jdbc2;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 *     Wrap a {@link ResultSet}.
 * </p>
 */
public final class RowIterator implements AutoCloseable
{
    private final ResultSet delegate;

    public RowIterator (ResultSet delegate)
    {
        this.delegate = delegate;
    }

    /**
     * <p>
     *     Escape hatch.
     * </p>
     * @return the delegate
     */
    public ResultSet unwrap ()
    {
        return delegate;
    }

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

    public boolean next ()
    {
        try
        {
            return delegate.next();
        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }

    /**
     * <p>
     *     Map each method named {@code get<PropertyName>}
     *     to the column named {@code <columnNamePrefix><PropertyName>}.
     *     Data are read eagerly.
     *     The returned instance is independent of the {@link ResultSet}.
     * </p>
     * <p>
     *     This deliberately violates the Java Beans convention
     *     by not downcasing the first letter of the property name.
     * </p>
     * @param columnNamePrefix
     * a string that begins the column name
     * @param interfaceType
     * represent an interface
     * @param <T>
     * the interface type
     * @return
     * a proxy implementing the interface by mapping getters to columns
     */
    public <T> T read (String columnNamePrefix, Class<T> interfaceType)
    {
        return Model.of(interfaceType).read(columnNamePrefix, delegate);
    }
}
