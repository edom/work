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

    /**
     * <p>Map each column in a {@link ResultSet} to the public field of A having the same name.</p>
     * <p>Columns that do not have corresponding fields are silently ignored.</p>
     * <p>The class {@code A} must not have a field that has no same-named column in the {@code ResultSet};
     * otherwise unpacking will fail with an exception.</p>
     * <p>See {@link BasicUnpackPublicFieldFromMap#unpack(BasicMap)} for details.</p>
     * @param cls class of A
     * @param <A> unpacked representation type
     * @return a {@link BasicUnpackRow}
     */
    public static <A> BasicUnpackRow<A> columnToPublicField (Class<A> cls)
    {
        return new FreeBasicUnpackRowFromMap<>(BasicUnpackPublicFieldFromMap.of(cls));
    }
}
