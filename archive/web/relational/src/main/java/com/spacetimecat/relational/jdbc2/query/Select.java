package com.spacetimecat.relational.jdbc2.query;

import com.spacetimecat.relational.jdbc2.sync.TableDescribingInterface;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.stream.Collectors;

public final class Select extends AbstractTable
{
    private final List<String> columnNames;
    private final String tableName;
    private final String tableAlias;
    private final Class interfaceType;

    public Select (Class interfaceType, String tableName, String tableAlias)
    {
        this.interfaceType = interfaceType;
        this.columnNames = new TableDescribingInterface(interfaceType).getColumnNames();
        this.tableName = tableName;
        this.tableAlias = tableAlias;
    }

    public Table project (String... columns)
    {
        return new Project(this, Arrays.asList(columns));
    }

    @Override
    public String toSqlSelect ()
    {
        final String prefix = tableAlias + "_";
        final List<String> columnSpecs = columnNames.stream()
            .map(column -> String.format("%s.%s %s", tableAlias, column, prefix + column))
            .collect(Collectors.toList());
        if (columnSpecs.isEmpty())
        {
            throw new IllegalArgumentException(String.format("%s does not have any getter", interfaceType));
        }
        final String strColumnList = String.join(",", columnSpecs);
        return String.format("SELECT %s FROM %s %s", strColumnList, tableName, tableAlias);
    }

    @Override
    public List<String> getColumnNames ()
    {
        return Collections.emptyList();
    }
}
