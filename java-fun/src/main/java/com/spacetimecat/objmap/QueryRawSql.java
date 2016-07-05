package com.spacetimecat.objmap;

import com.spacetimecat.collection.Iterator;
import com.spacetimecat.function.BasicCheckedFunction1;

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
     * @param rawSql SQL code
     * @param use what to do with the rows
     * @param <R> the return type of {@code use} and this method
     *
     * @return the return value of {@code use}
     */
    <R> R queryRawSql (String rawSql, BasicCheckedFunction1<? super Iterator<T>, R> use);
}
