package com.spacetimecat.collection;

/**
 * <p>Extend {@link BasicCollection} with {@link #add(Object) add}.</p>
 * @param <A> element type
 */
public interface BasicMutableCollection<A> extends BasicCollection<A>
{
    /**
     * <p>Add the element to this collection.</p>
     * @param a the newcomer
     * @return this
     */
    BasicMutableCollection<A> add (A a);
}
