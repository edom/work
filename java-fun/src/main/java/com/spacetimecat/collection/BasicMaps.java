package com.spacetimecat.collection;

import java.sql.ResultSet;

public final class BasicMaps
{
    private BasicMaps () {}

    public static BasicMap<String, Object> from (ResultSet r)
    {
        return new BasicMapFromResultSet(r);
    }
}
