package com.spacetimecat.relational.jdbc2.query;

import java.util.List;

final class Project extends AbstractTable
{
    private final Table table;
    private final List<String> columns;

    Project (Table table, List<String> columns)
    {
        this.table = table;
        this.columns = columns;
    }

    @Override
    public String toSqlSelect ()
    {
        return String.format("SELECT %s FROM (%s) unnamed", String.join(", ", columns), table.toSqlSelect());
    }

    @Override
    public List<String> getColumnNames ()
    {
        return columns;
    }
}
