package com.spacetimecat.objmap;

import java.sql.ResultSet;

/**
 * <p>This transforms a row in {@link ResultSet} to {@code A}.</p>
 */
public interface BasicUnpackRow<A> extends BasicUnpack<ResultSet, A>
{
    /**
     * <p>Deserialize an {@code A} from the current row of the {@link ResultSet}
     * without mutating the {@link ResultSet}.</p>
     *
     * <p>This can assume that {@link ResultSet#next()}
     * has been called before and has returned true.</p>
     *
     * @return cannot be null
     */
    @Override
    A unpack (ResultSet r);
}
