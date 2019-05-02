package com.spacetimecat.collection;

import com.spacetimecat.function.BasicFunction1;

/**
 * <p>Would have been {@code Indexed<int, V>} if not for Java's type system.</p>
 *
 * @param <V> element type
 */
public interface IntegerIndexed<V> extends Indexed<Integer, V>
{
    /**
     * <p>If the class also implements {@link BasicFunction1},
     * it should forward {@link #at(Object)} to this method.</p>
     *
     * @param i index
     *
     * @return the element at the given index
     */
    V at (int i);
}
