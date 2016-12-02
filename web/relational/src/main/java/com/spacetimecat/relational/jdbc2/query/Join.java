package com.spacetimecat.relational.jdbc2.query;

import java.util.ArrayList;
import java.util.List;

final class Join extends AbstractTable
{
    private final Table left;
    private final Table right;

    Join (Table left, Table right)
    {
        this.left = left;
        this.right = right;
    }

    @Override
    public String toSqlSelect ()
    {
        return String.format("SELECT * FROM (%s) theLeft INNER JOIN (%s) theRight", left.toSqlSelect(), right.toSqlSelect());
    }

    @Override
    public List<String> getColumnNames ()
    {
        final List<String> result = new ArrayList<>(left.getColumnNames().size() + right.getColumnNames().size());
        result.addAll(left.getColumnNames());
        result.addAll(right.getColumnNames());
        return result;
    }
}
