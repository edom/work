package com.spacetimecat.relational.jdbc2;

import java.io.PrintStream;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

public final class DumpResultSet
{
    private final ResultSet resultSet;

    public DumpResultSet (ResultSet resultSet)
    {
        this.resultSet = resultSet;
    }

    public void to (PrintStream out)
    {
        try
        {
            final ResultSetMetaData meta = resultSet.getMetaData();
            final int n = meta.getColumnCount();
            for (int i = 1; i <= n; ++i)
            {
                out.printf("%s | ", meta.getColumnName(i));
            }
            out.println();
            while (resultSet.next())
            {
                for (int i = 1; i <= n; ++i)
                {
                    out.printf("%s | ", resultSet.getObject(i));
                }
                out.println();
            }

        }
        catch (SQLException e)
        {
            throw new JdbcException(e);
        }
    }
}
