package com.spacetimecat.relational.jdbc2.sync;

public final class JdbcColumn
{
    private final JdbcMethod method;

    public JdbcColumn (JdbcMethod method)
    {
        if (!method.describesColumn())
        {
            throw new IllegalArgumentException("!method.describesColumn()");
        }
        this.method = method;
    }

    public int getOrder ()
    {
        return method.getOrder();
    }

    public boolean isPrimaryKey ()
    {
        return method.isPrimaryKey();
    }

    public String name ()
    {
        return method.getColumnName();
    }

    public Class javaType ()
    {
        return method.getReturnType();
    }

    public String sqlType ()
    {
        final Class t = javaType();
        if (t == int.class || t == Integer.class) { return "INTEGER"; }
        if (t == long.class || t == Long.class) { return "BIGINT"; }
        if (t == String.class) { return "VARCHAR"; }
        throw new UnsupportedOperationException(t.toString());
    }

    public String definitionClause ()
    {
        final String constraint = isPrimaryKey() ? String.format(", PRIMARY KEY (%s)", name()) : "";
        return String.format("%s %s NOT NULL%s", name(), sqlType(), constraint);
    }
}
