package com.spacetimecat.objmap;

import com.spacetimecat.collection.RowIterator;

/**
 * <p>Each instance of this can deserialize the result of
 * a SQL query that produces the expected columns.</p>
 */
public interface QueryRawSql<T>
{
    /**
     * <p>Unsafely run the SQL code without sanitizing it,
     * and wrap the {@link java.sql.ResultSet}.</p>
     *
     * <p>The SQL code must produce exactly one {@link java.sql.ResultSet}.</p>
     *
     * <p>Remember to {@link RowIterator#close()} the returned iterator.</p>
     *
     * @param rawSql SQL code
     *
     * @return a view that has to be closed later
     */
    RowIterator<T> queryRawSql (String rawSql);
}
