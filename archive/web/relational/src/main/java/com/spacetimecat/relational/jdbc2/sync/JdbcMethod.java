package com.spacetimecat.relational.jdbc2.sync;

import com.spacetimecat.relational.jdbc2.example.Order;

import javax.persistence.Column;
import javax.persistence.Id;
import java.lang.reflect.Method;

public final class JdbcMethod
{
    private final Method method;
    private final TableDescribingInterface table;

    public JdbcMethod (Method method)
    {
        this.method = method;
        this.table = new TableDescribingInterface(method.getDeclaringClass());
    }

    public Class getReturnType ()
    {
        return method.getReturnType();
    }

    public boolean isPrimaryKey ()
    {
        return method.isAnnotationPresent(Id.class);
    }

    public boolean describesColumn ()
    {
        final boolean isAnnotated = method.isAnnotationPresent(Column.class);
        final boolean isGetter = method.getName().startsWith("get")
            && method.getParameterCount() == 0
            && method.getReturnType() != void.class;
        return isAnnotated || isGetter;
    }

    public int getOrder ()
    {
        final Order annotation = method.getAnnotation(Order.class);
        if (annotation == null) { return 0; }
        return annotation.value();
    }

    public String getPropertyName ()
    {
        final String methodName = method.getName();
        if (!methodName.startsWith("get"))
        {
            throw new IllegalArgumentException(methodName);
        }
        return methodName.substring(3);
    }

    public String getColumnName ()
    {
        final Column column = method.getAnnotation(Column.class);
        if (column != null)
        {
            return column.name();
        }
        final String methodName = method.getName();
        if (!methodName.startsWith("get"))
        {
            return null;
        }
        if (methodName.length() <= 3)
        {
            return "";
        }
        return methodName.substring(3);
    }

    public String getSchemaName ()
    {
        return table.getSchemaName();
    }

    public String getTableName ()
    {
        return table.getTableName();
    }
}
