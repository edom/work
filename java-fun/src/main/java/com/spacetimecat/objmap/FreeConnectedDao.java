package com.spacetimecat.objmap;

import com.spacetimecat.Limbos;
import com.spacetimecat.MutableLimbo;
import com.spacetimecat.collection.*;

import javax.sql.DataSource;
import java.sql.*;

final class FreeConnectedDao<T> implements ConnectedDao<T>
{
    private final DataSource db;
    private final SqlSelectPrefix ssp;
    private final BasicUnpackRow<T> bur;

    FreeConnectedDao (DataSource db, SqlSelectPrefix ssp, BasicUnpackRow<T> bur)
    {
        this.db = db;
        this.ssp = ssp;
        this.bur = bur;
    }

    @Override
    public RowIterator<T> findAll (String keyColumn, Object key)
    {
        final MutableLimbo limbo = Limbos.open();
        final String sql = ssp.sqlSelectPrefix() + " WHERE " + keyColumn + " = ?";
        try
        {
            final Connection c = limbo.register(db.getConnection());
            final PreparedStatement s = limbo.register(c.prepareStatement(sql));
            s.setObject(1, key);
            final ResultSet r = limbo.register(s.executeQuery());
            return Iterators.from(bur, limbo.immutable(), r);
        }
        catch (SQLException e)
        {
            return limbo.closeAndThrow(new ReadException(e));
        }
    }

    @Override
    public RowIterator<T> queryRawSql (String rawSql)
    {
        final MutableLimbo limbo = Limbos.open();
        try
        {
            final Connection c = limbo.register(db.getConnection());
            final Statement s = limbo.register(c.createStatement());
            final ResultSet r = limbo.register(s.executeQuery(rawSql));
            return Iterators.from(bur, limbo.immutable(), r);
        }
        catch (SQLException e)
        {
            return limbo.closeAndThrow(new ReadException(e));
        }
    }
}
