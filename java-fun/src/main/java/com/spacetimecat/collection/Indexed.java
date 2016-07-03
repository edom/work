package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>{@link BasicFunction1} with some constraints on nulls.</p>
 *
 * @param <I> index type
 * @param <V> element type
 *
 * @see BasicMap
 * @see IntegerIndexed
 */
public interface Indexed<I, V> extends BasicFunction1<I, V>
{
    /**
     * {@inheritDoc}
     * @param i cannot be null
     * @return null if the index is out of bounds
     */
    @Override
    V at (I i);
}
