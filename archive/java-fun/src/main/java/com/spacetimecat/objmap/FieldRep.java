package com.spacetimecat.objmap;

import com.spacetimecat.collection.BasicMap;
import com.spacetimecat.objmap.annotation.Field;

import java.lang.reflect.Constructor;
import java.lang.reflect.Parameter;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;

final class FieldRep
{
    final Class<?> type;
    final String name;

    FieldRep (Class<?> type, String name)
    {
        this.type = type;
        this.name = name;
    }

    Object readFrom (BasicMap<String, Object> map)
    {
        return map.get(name);
    }

    Object readFrom (ResultSet row)
    {
        try
        {
            return new ExtResultSet(row).getObject(name, type);
        }
        catch (SQLException e)
        {
            throw new ReadException("Error reading column: " + name, e);
        }
    }

    static FieldRep from (Parameter p)
    {
        final Field c = p.getAnnotation(Field.class);
        if (c == null)
        {
            throw new IllegalArgumentException("Parameter must have Field annotation: " + p);
        }
        final String name = c.value();
        final Class<?> type = p.getType();
        return new FieldRep(type, name);
    }

    static List<FieldRep> listFrom (Constructor c)
    {
        final Parameter[] params = c.getParameters();
        final List<FieldRep> result = new ArrayList<>();
        for (final Parameter p : params)
        {
            result.add(FieldRep.from(p));
        }
        return result;
    }
}
