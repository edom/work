package com.spacetimecat.objmap;

import com.spacetimecat.Limbos;
import com.spacetimecat.collection.Iterator;
import com.spacetimecat.collection.Iterators;
import com.spacetimecat.function.BasicCheckedFunction1;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.Statement;

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
}
