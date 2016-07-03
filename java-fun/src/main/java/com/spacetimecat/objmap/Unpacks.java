package com.spacetimecat.objmap;

import com.spacetimecat.collection.BasicMap;

import java.sql.ResultSet;

/**
 * <p>Make unpackers.</p>
 */
public final class Unpacks
{
    public static <A> BasicUnpack<ResultSet, A> resultSet (Class<A> cls)
    {
        return BasicUnpackRowUsingConstructor.of(cls);
    }

    public static <A> BasicUnpack<BasicMap<String, Object>, A> map (Class<A> cls)
    {
        throw new UnsupportedOperationException();
//        return new BasicUnpackFieldMap<>(cls);
    }
}
