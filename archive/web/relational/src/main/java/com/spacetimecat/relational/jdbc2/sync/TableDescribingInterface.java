package com.spacetimecat.relational.jdbc2.sync;

import javax.persistence.Table;
import java.lang.reflect.Method;
import java.util.*;
import java.util.stream.Collectors;

public final class TableDescribingInterface
{
    private final Class cls;
    private List<String> columnNames;

    public TableDescribingInterface (Class cls)
    {
        this.cls = cls;
    }

    private Table getTableAnnotation ()
    {
        return (Table) cls.getAnnotation(Table.class);
    }

    public String getSchemaName ()
    {
        final Table table = getTableAnnotation();
        if (table == null)
        {
            return null;
        }
        return table.schema();
    }

    public String getTableName ()
    {
        final Table table = getTableAnnotation();
        if (table == null)
        {
            return null;
        }
        return table.name();
    }

    public List<JdbcMethod> getters ()
    {
        final List<JdbcMethod> result = new ArrayList<>();
        for (final Method method : cls.getMethods())
        {
            final JdbcMethod jm = new JdbcMethod(method);
            if (jm.describesColumn())
            {
                result.add(jm);
            }
        }
        return result;
    }

    public List<JdbcColumn> columns ()
    {
        return getters().stream().map(JdbcColumn::new).collect(Collectors.toList());
    }

    public String getDataDefinition ()
    {
        final List<JdbcColumn> columns = columns();
        Collections.sort(columns, new PrimaryKeysFirstAndThenByOrderAnnotation());
        final List<String> columnSpecs = columns.stream().map(JdbcColumn::definitionClause).collect(Collectors.toList());
        return String.format("CREATE TABLE %s (%s)", getTableName(), String.join(", ", columnSpecs));
    }

    public List<String> getColumnNames ()
    {
        return columns().stream().map(JdbcColumn::name).collect(Collectors.toList());
    }

    private static class PrimaryKeysFirstAndThenByOrderAnnotation implements Comparator<JdbcColumn>
    {
        @Override
        public int compare (JdbcColumn a, JdbcColumn b)
        {
            final int pk = Boolean.compare(a.isPrimaryKey(), b.isPrimaryKey());
            if (pk != 0) { return -pk; }
            return Integer.compare(a.getOrder(), b.getOrder());
        }
    }
}
