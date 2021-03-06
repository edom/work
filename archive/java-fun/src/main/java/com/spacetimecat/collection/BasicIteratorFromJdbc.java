package com.spacetimecat.collection;

import com.spacetimecat.UncheckedException;
import com.spacetimecat.objmap.BasicUnpackRow;

import java.sql.ResultSet;
import java.sql.SQLException;

final class BasicIteratorFromJdbc<A> implements BasicIterator<A>
{
    private final BasicUnpackRow<A> rd;
    private final ResultSet rs;

    BasicIteratorFromJdbc (BasicUnpackRow<A> rd, ResultSet rs)
    {
        this.rd = rd;
        this.rs = rs;
    }

    @Override
    public A next ()
    {
        try
        {
            if (!rs.next()) { return null; }
            final A a = rd.unpack(rs);
            if (a == null) { throw new NullPointerException(); }
            return a;
        }
        catch (SQLException e)
        {
            throw new UncheckedException(e);
        }
    }
}
