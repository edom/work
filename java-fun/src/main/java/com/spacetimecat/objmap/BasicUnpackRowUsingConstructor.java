package com.spacetimecat.objmap;

import com.spacetimecat.objmap.annotation.Table;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.ResultSet;
import java.util.Collections;
import java.util.List;

/**
 * <p>An instance of this class describes the relationship
 * between one SQL table and one Java class.</p>
 *
 * @param <J> the Java type whose inhabitant corresponds to one row in the SQL table
 */
final class BasicUnpackRowUsingConstructor<J> implements SqlSelectPrefix, BasicUnpackRow<J>
{
    private final Constructor<J> constructor;
    private final List<FieldRep> columns;
    private final String className;
    private final String table;

    BasicUnpackRowUsingConstructor (Constructor<J> c, String table, List<FieldRep> columns)
    {
        this.constructor = c;
        this.table = table;
        this.className = c.getDeclaringClass().getName();
        this.columns = columns;
    }

    static <T> BasicUnpackRowUsingConstructor<T> of (Class<T> cls)
    {
        final Table a = cls.getAnnotation(Table.class);
        // You can use this without a Table annotation,
        // but calling sqlSelectPrefix will throw NullPointerException.
        final String table = (a != null) ? a.value() : null;
        final Constructor<T> cons = (Constructor<T>) new ExtClass<>(cls).findSuitableForRowMap();
        final List<FieldRep> columns = Collections.unmodifiableList(FieldRep.listFrom(cons));
        return new BasicUnpackRowUsingConstructor<>(cons, table, columns);
    }

    @Override
    public J unpack (ResultSet resultSet)
    {
        final Object[] args = new Object[columns.size()];
        {
            int i = 0;
            for (final FieldRep c : columns)
            {
                args[i++] = c.readFrom(resultSet);
            }
        }
        try
        {
            return (J) constructor.newInstance(args);
        }
        catch (IllegalAccessException e)
        {
            throw new ReadException(className + ": matching constructor must be public", e);
        }
        catch (InstantiationException e)
        {
            throw new ReadException(className + ": instance creation failed", e);
        }
        catch (ExceptionInInitializerError e)
        {
            throw new ReadException(className + ": instance initialization failed", e.getCause());
        }
        catch (InvocationTargetException e)
        {
            throw new ReadException(e.getCause());
        }
    }

    private String sqlColumnList ()
    {
        final StringBuilder s = new StringBuilder();
        for (final FieldRep c : columns)
        {
            s.append(", ").append(c.name);
        }
        return s.substring(2);
    }

    @Override
    public String sqlSelectPrefix ()
    {
        return "SELECT " + sqlColumnList() + " FROM " + table;
    }
}
