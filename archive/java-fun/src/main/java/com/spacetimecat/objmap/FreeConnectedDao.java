package com.spacetimecat.objmap;

import com.spacetimecat.Limbos;
import com.spacetimecat.UncheckedException;
import com.spacetimecat.collection.FiniteIterable;
import com.spacetimecat.collection.Iterator;
import com.spacetimecat.collection.Iterators;
import com.spacetimecat.function.BasicCheckedFunction1;

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
    public <R> R findAll (String keyColumn, Object key, BasicCheckedFunction1<? super Iterator<T>, R> use)
    {
        return Limbos.with(limbo ->
        {
            final String sql = ssp.sqlSelectPrefix() + " WHERE " + keyColumn + " = ?";
            final Connection c = limbo.register(db.getConnection());
            final PreparedStatement s = limbo.register(c.prepareStatement(sql));
            s.setObject(1, key);
            final ResultSet r = limbo.register(s.executeQuery());
            final Iterator<T> it = Iterators.from(bur, r);
            return use.at(it);
        });
    }

    @Override
    public <R> R queryRawSql (String rawSql, BasicCheckedFunction1<? super Iterator<T>, R> use)
    {
        return Limbos.with(limbo ->
        {
            final Connection c = limbo.register(db.getConnection());
            final Statement s = limbo.register(c.createStatement());
            final ResultSet r = limbo.register(s.executeQuery(rawSql));
            final Iterator<T> it = Iterators.from(bur, r);
            return use.at(it);
        });
    }

    public void insert (FiniteIterable<T> list)
    {
        Limbos.with_(limbo ->
        {
            final String tableName = "FIXME";
            final int columnCount = 0; // FIXME
            final String columnList = "FIXME"; // FIXME
            StringBuilder sql = new StringBuilder()
                .append("INSERT INTO ").append(tableName).append(' ').append(columnList).append(' ').append(" VALUES (");
            for (int i = 0; i < columnCount; ++i)
            {
                if (i > 0) { sql = sql.append(','); }
                sql = sql.append("?");
            }
            sql = sql.append(')');
            final Connection c = limbo.register(db.getConnection());
            final boolean ac = c.getAutoCommit();
            try
            {
                c.setAutoCommit(false);
                final PreparedStatement s = limbo.register(c.prepareStatement(sql.toString()));
                list.forEach(r ->
                {
                    try
                    {
                        // s.setObject();
                        s.addBatch();
                    }
                    catch (SQLException e)
                    {
                        throw new UncheckedException(e);
                    }
                });
                s.executeBatch();
                c.commit();
            }
            finally
            {
                c.setAutoCommit(ac);
            }
        });
    }
}
