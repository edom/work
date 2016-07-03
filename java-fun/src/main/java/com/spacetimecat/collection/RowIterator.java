package com.spacetimecat.collection;

import com.spacetimecat.Bracket;
import com.spacetimecat.UncheckedException;
import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicProcedure1;
import com.spacetimecat.objmap.BasicUnpackRow;

/**
 * <p>{@link Iterator} wrapping JDBC {@link java.sql.ResultSet}.</p>
 *
 * <p>This does <em>not</em> automatically {@link RowIterator#close()}
 * the underlying {@link java.sql.ResultSet} if there are no more inputs.</p>
 *
 * <p>Use {@link #with(BasicFunction1)} to ensure that resources are properly freed.</p>
 *
 * @see java.sql.ResultSet
 * @see BasicUnpackRow
 */
public interface RowIterator<A> extends
    AutoCloseable
    , Bracket<RowIterator<A>>
    , Iterator<A>
{
    /**
     * <p>Close the underlying {@link java.sql.ResultSet},
     * {@link java.sql.Statement},
     * and {@link java.sql.Connection}.</p>
     */
    @Override
    void close () throws UncheckedException;

    RowIterator<A> with (BasicProcedure1<RowIterator<A>> f);

    /**
     * <p>Pass this to f, and {@link #close() close} this after f returns,
     * even if f throws something.</p>
     */
    <B> B with (BasicFunction1<RowIterator<A>, B> f);
}
