package com.spacetimecat.objmap;

/**
 * <p>Make Data Access Objects.</p>
 */
public final class Daos
{
    public static <T> Dao<T> of (Class<T> cls)
    {
        final BasicUnpackRowUsingConstructor<T> rruc = BasicUnpackRowUsingConstructor.of(cls);
        return new FreeDao<>(rruc, rruc);
    }

    public static <T> Dao<T> columnToPublicField (Class<T> cls)
    {
        return new FreeDao<>(null, Unpacks.columnToPublicField(cls));
    }
}
