package com.spacetimecat.objmap;

import java.sql.ResultSet;
import java.sql.SQLException;

final class ExtResultSet implements AutoCloseable
{
    private final ResultSet inner;

    ExtResultSet (ResultSet inner)
    {
        this.inner = inner;
    }

    public Object getObject (String column, Class<?> type) throws SQLException
    {
        if (type.equals(int.class)) { return inner.getInt(column); }
        if (type.equals(long.class)) { return inner.getLong(column); }
        if (type.equals(String.class)) { return inner.getString(column); }
        throw new UnsupportedOperationException("ext_getObject: not implemented: " + type.getName());
    }

    @Override
    public void close () throws SQLException
    {
        inner.close();
    }
}
