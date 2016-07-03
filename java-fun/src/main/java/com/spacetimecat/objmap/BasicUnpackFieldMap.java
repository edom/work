package com.spacetimecat.objmap;

import com.spacetimecat.collection.BasicMap;
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
final class BasicUnpackFieldMap<J> implements BasicUnpack<BasicMap<String, Object>, J>
{
    private final Class<J> cls;
    private final List<FieldRep> columns;
    private final Constructor<J> constructor;

    BasicUnpackFieldMap (Class<J> cls, Constructor<J> c, List<FieldRep> columns)
    {
        this.cls = cls;
        this.constructor = c;
        this.columns = columns;
    }

    static <T> BasicUnpackFieldMap<T> of (Class<T> cls)
    {
        final Table a = cls.getAnnotation(Table.class);
        // You can use this without a Table annotation,
        // but calling sqlSelectPrefix will throw NullPointerException.
        final String table = (a != null) ? a.value() : null;
        final Constructor<T> cons = (Constructor<T>) new ExtClass<>(cls).findSuitableForRowMap();
        final List<FieldRep> columns = Collections.unmodifiableList(FieldRep.listFrom(cons));
        return new BasicUnpackFieldMap<>(cls, cons, columns);
    }

    @Override
    public J unpack (BasicMap<String, Object> map)
    {
        final String className = cls.getName();
        final Object[] args = new Object[columns.size()];
        {
            int i = 0;
            for (final FieldRep c : columns)
            {
                args[i++] = c.readFrom(map);
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
}
