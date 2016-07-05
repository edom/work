package com.spacetimecat.collection;

import com.spacetimecat.Bracket;
import com.spacetimecat.UncheckedException;
import com.spacetimecat.function.BasicFunction1;
import com.spacetimecat.function.BasicProcedure1;
import com.spacetimecat.objmap.BasicUnpackRow;

import java.io.Closeable;

/**
 * <p>{@link Iterator} wrapping JDBC {@link java.sql.ResultSet}.</p>
 *
 * <p>This does <em>not</em> automatically {@link RowIterator#close()}
 * the underlying {@link java.sql.ResultSet} if there are no more inputs.</p>
 *
 * <h2>Preventing resource leaks</h2>
 *
 * <p>A {@link RowIterator} owns a ResultSet, its Statement, and its Connection.</p>
 *
 * <p>Use {@link Bracket#withFunction(BasicFunction1)}
 * or {@link Bracket#withProcedure(BasicProcedure1)}
 * to ensure that resources are properly freed.</p>
 *
 * @see java.sql.ResultSet
 * @see BasicUnpackRow
 */
public interface RowIterator<A> extends
    Bracket<RowIterator<A>>
    , Closeable
    , Iterator<A>
{
    /**
     * <p>Close the underlying {@link java.sql.ResultSet},
     * {@link java.sql.Statement},
     * and {@link java.sql.Connection}.</p>
     *
     * <p>This method is idempotent.
     * After the first call, this method does nothing.</p>
     */
    @Override
    void close () throws UncheckedException;

    RowIterator<A> withProcedure (BasicProcedure1<? super RowIterator<A>> f);

    /**
     * <p>Pass this to f, and {@link #close() close} this after f returns,
     * even if f throws something.</p>
     * @param f uses this object
     */
    <B> B withFunction (BasicFunction1<? super RowIterator<A>, B> f);
}
