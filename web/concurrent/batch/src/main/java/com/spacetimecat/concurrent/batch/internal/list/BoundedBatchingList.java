package com.spacetimecat.concurrent.batch.internal.list;

import java.util.ArrayList;
import java.util.List;

/**
 * <p>
 *     A {@link BatchingList} with limited capacity.
 * </p>
 *
 * @param <A>
 * element type
 */
public final class BoundedBatchingList<A> implements BatchingList<A>
{
    private final int capacity;
    private List<A> list;

    public BoundedBatchingList (int capacity)
    {
        this.capacity = capacity;
        this.list = new ArrayList<>(capacity);
    }

    @Override
    public boolean add (A thing)
    {
        if (list.size() >= capacity)
        {
            return false;
        }
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
        this.list = new ArrayList<>(capacity);
        return list;
    }

    @Override
    public int size ()
    {
        return list.size();
    }
}
