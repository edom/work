package com.spacetimecat.concurrent.batch.internal.list;

import java.util.List;

/**
 * <p>
 *     The instance does not have to be thread safe;
 *     the caller is responsible for limiting
 *     concurrent access to the instance.
 * </p>
 *
 * @param <A>
 * element type
 */
public interface BatchingList<A>
{
    boolean add (A thing);

    boolean isEmpty ();

    List<A> reap ();

    int size ();
}
