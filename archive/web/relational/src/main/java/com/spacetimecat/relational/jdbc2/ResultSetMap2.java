package com.spacetimecat.relational.jdbc2;

import com.spacetimecat.relational.dyno.Map2;
import com.spacetimecat.relational.dyno.PropertyNotFoundException;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * <p>
 *     A {@link Map2} backed by a {@link java.sql.ResultSet}.
 * </p>
 */
public final class ResultSetMap2 implements Map2
{
    private final ResultSet delegate;

    public ResultSetMap2 (ResultSet delegate)
    {
        this.delegate = delegate;
    }

    @Override
    public Object get (String key)
    {
        try
        {
            return delegate.getObject(key);
        }
        catch (SQLException e)
        {
            throw new PropertyNotFoundException(e);
        }
    }
}
