package com.spacetimecat.collection;

/**
 * <p>{@link Dumpable} that can allocate a {@link java.util.List} on its own.</p>
 *
 * @param <A> element type
 */
public interface ToNewStdList<A>
{
    /**
     * <p>This copies this collection to a new list
     * that is then returned.</p>
     *
     * <p>You can do whatever you want to the returned list.
     * It will not affect the original.</p>
     *
     * @return a new {@link java.util.List} that is a shallow copy of this
     */
    java.util.List<A> toNewStdList ();
}
