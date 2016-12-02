package com.spacetimecat.relational.jdbc2.query;

import java.util.List;

final class Filter extends AbstractTable
{
    private final Table table;
    private final Predicate2 predicate;

    Filter (Table table, Predicate2 predicate)
    {
        if (table == null) { throw new NullPointerException("table"); }
        if (predicate == null) { throw new NullPointerException("predicate"); }
        this.table = table;
        this.predicate = predicate;
    }

    @Override
    public String toSqlSelect ()
    {
        return String.format("SELECT * FROM (%s) unnamed WHERE %s", table.toSqlSelect(), predicate.toSqlExpression());
    }

    @Override
    public List<String> getColumnNames ()
    {
        return null;
    }
}
