package com.spacetimecat.relational.jdbc2.query;

import java.util.List;

/**
 * <p>
 *     This participates in a query.
 * </p>
 */
public interface Table
{
    Table join (Table that);

    Table filter (Predicate2 predicate);

    String toSqlSelect ();

    List<String> getColumnNames ();
}
