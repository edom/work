package com.spacetimecat.objmap;

import javax.sql.DataSource;

/**
 * <p>Data Access Object.</p>
 */
final class FreeDao<T> implements Dao<T>, SqlSelectPrefix
{
    private final SqlSelectPrefix ssp;
    private final BasicUnpackRow<T> bur;

    FreeDao (SqlSelectPrefix ssp, BasicUnpackRow<T> bur)
    {
        this.ssp = ssp;
        this.bur = bur;
    }

    @Override
    public String sqlSelectPrefix ()
    {
        return ssp.sqlSelectPrefix();
    }

    @Override
    public ConnectedDao<T> connect (DataSource db)
    {
        return new FreeConnectedDao<>(db, ssp, bur);
    }
}
