package com.spacetimecat.concurrent.batch.internal.list;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 *     A {@link BatchingList} that automatically expands as things are added.
 * </p>
 *
 * @param <A>
 * element type
 */
public final class DefaultBatchingList<A> implements BatchingList<A>
{
    private final int initialCapacity;

    private List<A> list;

    public DefaultBatchingList (int initialCapacity)
    {
        this.initialCapacity = initialCapacity;
        this.list = new ArrayList<>(initialCapacity);
    }

    @Override
    public boolean add (A thing)
    {
        return list.add(thing);
    }

    @Override
    public boolean isEmpty ()
    {
        return list.isEmpty();
    }

    @Override
    public List<A> reap ()
    {
        final List<A> list = this.list;
        this.list = new ArrayList<>(initialCapacity);
        return list;
    }

    @Override
    public int size ()
    {
        return list.size();
    }
}
