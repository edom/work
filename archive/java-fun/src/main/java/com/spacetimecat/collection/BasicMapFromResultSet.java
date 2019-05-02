package com.spacetimecat.collection;

import com.spacetimecat.UncheckedException;

import java.sql.ResultSet;
import java.sql.SQLException;

final class BasicMapFromResultSet implements BasicMap<String, Object>
{
    private final ResultSet rs;

    BasicMapFromResultSet (ResultSet rs)
    {
        this.rs = rs;
    }

    @Override
    public Object get (String s)
    {
        try
        {
            return rs.getObject(s);
        }
        catch (SQLException e)
        {
            throw new UncheckedException(e);
        }
    }
}
